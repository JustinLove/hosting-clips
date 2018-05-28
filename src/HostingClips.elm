module HostingClips exposing (..)

import Twitch.Helix.Decode as Helix
import Twitch.Tmi.Decode as Tmi
import Twitch.Helix as Helix
import TwitchId
import View exposing (Clip, Host)

import Html
import Navigation exposing (Location)
import Http
import Time exposing (Time)
import Dict exposing (Dict)
import Json.Decode

requestLimit = 100
rateLimit = 30
requestRate = 60*Time.second/rateLimit

type Msg
  = User (Result Http.Error (List Helix.User))
  | Hosts (Result Http.Error (List Tmi.Host))
  | Clips (Result Http.Error (List Helix.Clip))
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
  , clips : Dict String (List Clip)
  , displayedBroadcaster : Maybe String
  , displayedClip : Maybe Clip
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
    rhosts = Json.Decode.decodeString Tmi.hosts Tmi.sampleHost |> Result.mapError (\err -> Http.BadPayload err {url = "", status = {code = 200, message = ""}, headers = Dict.empty, body = ""})
  in
  update (Hosts rhosts)
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
    , clips = Dict.empty
    , displayedBroadcaster = Just "x"
    , displayedClip = Nothing
    , pendingRequests =
      [ case mlogin of
          Just login -> fetchUserByName login
          Nothing -> Cmd.none
      ] |> List.filter (\c -> c /= Cmd.none)
    , outstandingRequests = 1
    }

update msg model =
  case msg of
    User (Ok (user::_)) ->
      ( { model
        | login = Just user.login
        , userId = Just user.id
        , pendingRequests = List.append model.pendingRequests
          []
        }
      , if (Just user.id) /= model.userId then
          Navigation.modifyUrl (model.location.pathname ++ "?userId="  ++ user.id)
        else
          Cmd.none
      )
    User (Ok _) ->
      let _ = Debug.log "user did not find that login name" "" in
      (model, Cmd.none)
    User (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Clips (Ok twitchClips) ->
      let clips = List.map myClip twitchClips in
      ( { model
        | clips = Dict.union (groupBy .broadcasterId clips) model.clips
        , displayedClip = List.head clips
        , pendingRequests = List.append model.pendingRequests
          []
        }
      , Cmd.none
      )
    Clips (Err error) ->
      let _ = Debug.log "clip fetch error" error in
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
    ]

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
    , tagger = Response << Clips
    , url = (fetchClipsUrl id)
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

groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy attr list =
  List.foldl (\x dict ->
      Dict.update (attr x) (Maybe.withDefault [] >> (::) x >> Just) dict
    ) Dict.empty list
