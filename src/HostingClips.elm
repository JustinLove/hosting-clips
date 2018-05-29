module HostingClips exposing (..)

import Twitch.Helix.Decode as Helix
import Twitch.Tmi.Decode as Tmi
import Twitch.Helix as Helix
import TwitchId
import View exposing (Choice(..), Clip, Host)

import Html
import Navigation exposing (Location)
import Http
import Time exposing (Time)
import Dict exposing (Dict)
import Json.Decode
import Array exposing (Array)
import Random

requestLimit = 100
rateLimit = 30
requestRate = 60*Time.second/rateLimit
clipCycleTime = 60*Time.second
noClipCycleTime = 10*Time.second

type Msg
  = User (Result Http.Error (List Helix.User))
  | Hosts (Result Http.Error (List Tmi.Host))
  | Clips String (Result Http.Error (List Helix.Clip))
  | Pick Int
  | NextChoice Time
  | Response Msg
  | NextRequest Time
  | CurrentUrl Location
  | UI (View.Msg)

type alias Model =
  { location : Location
  , login : Maybe String
  , userId : Maybe String
  , showClip : Bool
  , hostLimit : Int
  , hosts : List Host
  , clips : Array Choice
  , thanks : Choice
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

main = Navigation.program CurrentUrl
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : Location -> (Model, Cmd Msg)
init location =
  let
    mlogin = Debug.log "Login" <| extractSearchArgument "login" location
    muserId = Debug.log "userId" <| extractSearchArgument "userId" location
    mshowClip = Debug.log "showClip" <| extractSearchArgument "showClip" location
    mhostLimit = Debug.log "hostLimit" <| extractSearchArgument "hostLimit" location
  in
  update (NextRequest 0)
    { location = location
    , login = mlogin
    , userId = muserId
    , showClip = case Maybe.withDefault "true" mshowClip of
        "false" -> False
        _ -> True
    , hostLimit = mhostLimit
      |> Maybe.map String.toInt
      |> Maybe.withDefault (Err "unspecified")
      |> Result.withDefault requestLimit
    , hosts = []
    , clips = Array.empty
    , thanks = NoHosts
    , pendingRequests =
      (case muserId of
          Just id -> [ fetchHosts id, fetchUserById id ]
          Nothing ->
            case mlogin of
              Just login -> [ fetchUserByName login ]
              Nothing -> [ Cmd.none ]
      )
      |> List.filter (\c -> c /= Cmd.none)
    , outstandingRequests = 0
    }

update msg model =
  case msg of
    User (Ok (user::_)) ->
      ( { model
        | login = Just user.login
        , userId = Just user.id
        }
      , if (Just user.id) /= model.userId then
          Cmd.batch
            [ Navigation.modifyUrl (model.location.pathname ++ "?userId="  ++ user.id)
            , fetchHosts user.id
            ]
        else
          Cmd.none
      )
    User (Ok _) ->
      let _ = Debug.log "user did not find that login name" "" in
      (model, Cmd.none)
    User (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Hosts (Ok twitchHosts) ->
      let
        hosts = List.map myHost twitchHosts
        requests = hosts
          |> List.take model.hostLimit
          |> List.map (\{hostId} -> fetchClips hostId)
      in
      ( { model
        | hosts = hosts
        , pendingRequests = List.append model.pendingRequests requests
        }
      , Cmd.none
      )
    Hosts (Err error) ->
      let _ = Debug.log "hosts fetch error" error in
      (model, Cmd.none)
    Clips id (Ok []) ->
      let
        clips = Array.push (Thanks (displayNameForHost model.hosts id)) model.clips
      in
      ( { model | clips = clips }
      , maybePickCommand model.clips clips
      )
    Clips id (Ok twitchClips) ->
      let
        new = twitchClips
          |> List.map myClip
          |> List.map (\clip ->
              ThanksClip (displayNameForHost model.hosts clip.broadcasterId) clip
          )
        clips = Array.append model.clips (Array.fromList new)
      in
      ( { model | clips = clips }
      , maybePickCommand model.clips clips
      )
    Clips id (Err error) ->
      let _ = Debug.log "clip fetch error" error in
      (model, Cmd.none)
    Pick index ->
      ( { model
        | thanks = Array.get index model.clips
          |> Maybe.withDefault NoHosts
        }
      , Cmd.none
      )
    NextChoice time ->
      ( model, pickCommand model.clips )
    Response subMsg ->
      update subMsg { model | outstandingRequests = model.outstandingRequests - 1}
    NextRequest _ ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            }, next)
        _ -> (model, Cmd.none)
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    UI (View.None) ->
      ( model , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        Time.every (requestRate*1.05) NextRequest
    , if Array.isEmpty model.clips then
        Sub.none
      else
        case model.thanks of
          ThanksClip _ _ -> Time.every clipCycleTime NextChoice
          Thanks _ -> Time.every noClipCycleTime NextChoice
          NoHosts -> Time.every clipCycleTime NextChoice
    ]

maybePickCommand : Array a -> Array a -> Cmd Msg
maybePickCommand before after =
  if Array.isEmpty before then
    pickCommand after
  else
    Cmd.none

pickCommand : Array a -> Cmd Msg
pickCommand clips = 
  Random.generate Pick (Random.int 0 ((Array.length clips) - 1))

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> Cmd Msg
fetchUserByName login =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = Response << User
    , url = (fetchUserByNameUrl login)
    }

fetchUserByIdUrl : String -> String
fetchUserByIdUrl id =
  "https://api.twitch.tv/helix/users?id=" ++ id

fetchUserById : String -> Cmd Msg
fetchUserById id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = Response << User
    , url = (fetchUserByIdUrl id)
    }

fetchClipsUrl : String -> String
fetchClipsUrl id =
  "https://api.twitch.tv/helix/clips?broadcaster_id=" ++ id

fetchClips : String -> Cmd Msg
fetchClips id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.clips
    , tagger = Response << Clips id
    , url = (fetchClipsUrl id)
    }

fetchHostsUrl : String -> String
fetchHostsUrl id =
  "http://tmi.twitch.tv/hosts?include_logins=1&target=" ++ id

corsWorkaround : String -> String
corsWorkaround url =
  "https://cors-proxy.htmldriven.com/?url=" ++ (Http.encodeUri url)

corsUnwrap : Json.Decode.Decoder a -> Json.Decode.Decoder a
corsUnwrap decoder =
  Json.Decode.field "body" Json.Decode.string
    |> Json.Decode.andThen (\body ->
      case Json.Decode.decodeString decoder body of
        Ok a -> Json.Decode.succeed a
        Err err -> Json.Decode.fail err
      )

fetchHosts : String -> Cmd Msg
fetchHosts id =
  Http.send (Response << Hosts) <| Http.request
    { method = "GET"
    , headers = []
    , url = corsWorkaround (fetchHostsUrl id)
    , body = Http.emptyBody
    , expect = Http.expectJson (corsUnwrap Tmi.hosts)
    , timeout = Nothing
    , withCredentials = False
    }

myClip : Helix.Clip -> Clip
myClip clip =
  { id = clip.id
  , embedUrl = clip.embedUrl
  , broadcasterId = clip.broadcasterId
  }

myHost : Tmi.Host -> Host
myHost host =
  { hostId = host.hostId
  , hostDisplayName = host.hostDisplayName
  }

displayNameForHost : List Host -> String -> String
displayNameForHost hosts id =
  hosts
    |> List.filter (\host -> host.hostId == id)
    |> List.head
    |> Maybe.map .hostDisplayName
    |> Maybe.withDefault "Oops, how did that happen?"

extractSearchArgument : String -> Location -> Maybe String
extractSearchArgument key location =
  location.search
    |> String.dropLeft 1
    |> String.split "&"
    |> List.map (String.split "=")
    |> List.filter (\x -> case List.head x of
      Just s ->
        (String.toLower s) == (String.toLower key)
      Nothing ->
        False)
    |> List.head
    |> Maybe.andThen List.tail
    |> Maybe.andThen List.head
