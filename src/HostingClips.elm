module HostingClips exposing (..)

import Twitch.Helix.Decode as Helix
import Twitch.Tmi.Decode as Tmi
import Twitch.ClipsV2.Decode as ClipsV2
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
import Task
import Window

requestLimit = 100
rateLimit = 30
requestRate = 60*Time.second/rateLimit
clipCycleTime = 60*Time.second
noClipCycleTime = 10*Time.second

type Msg
  = User (Result Http.Error (List Helix.User))
  | Hosts (Result Http.Error (List Tmi.Host))
  | Clips String (Result Http.Error (List Helix.Clip))
  | ClipDetails (Result Http.Error ClipsV2.Clip)
  | Pick (Bool, Float)
  | NextChoice Time
  | Response Msg
  | NextRequest Time
  | CurrentUrl Location
  | WindowSize Window.Size
  | UI (View.Msg)

type alias Model =
  { location : Location
  , windowWidth : Int
  , windowHeight : Int
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
    mlogin = extractSearchArgument "login" location
    muserId = extractSearchArgument "userId" location
    mshowClip = extractSearchArgument "showClip" location
    mhostLimit = extractSearchArgument "hostLimit" location
  in
  ( { location = location
    , windowWidth = 852
    , windowHeight = 480
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
          Just id ->
            [ fetchUserById id
            , fetchHosts id
            , fetchClips 100 id
            ]
          Nothing ->
            case mlogin of
              Just login -> [ fetchUserByName login ]
              Nothing -> [ Cmd.none ]
      )
      |> List.filter (\c -> c /= Cmd.none)
    , outstandingRequests = 0
    }
  , Task.perform WindowSize Window.size
  )

update msg model =
  case msg of
    User (Ok (user::_)) ->
      let m2 =
        { model
        | login = Just user.login
        , userId = Just user.id
        }
      in
      ( m2
      , if (Just user.id) /= model.userId then
          Cmd.batch
            [ Navigation.modifyUrl (createPath m2)
            , fetchHosts user.id
            , fetchClips 100 user.id
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
          |> List.filter (\{hostDisplayName} ->
            not <| Array.foldl (\choice found ->
              found ||
              case choice of
                ThanksClip name _ -> name == hostDisplayName
                Thanks name -> name == hostDisplayName
                SelfClip _ -> False
                NoHosts -> False
              ) False model.clips
            )
          |> List.map (\{hostId} -> fetchClips 20 hostId)
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
      ( if (Just id) == model.userId then
          model
        else
          { model
          | clips = Array.push
            (Thanks (displayNameForHost model.hosts id))
            model.clips
          }
      , maybePickCommand model
      )
    Clips id (Ok twitchClips) ->
      let
        new = twitchClips
          |> List.map myClip
          |> List.map (\clip ->
              if (Just id) == model.userId then
                SelfClip clip
              else
                ThanksClip (displayNameForHost model.hosts clip.broadcasterId) clip
          )
        clips = Array.append model.clips (Array.fromList new)
      in
      ( { model | clips = clips }
      , maybePickCommand model
      )
    Clips id (Err error) ->
      let _ = Debug.log "clip fetch error" error in
      (model, Cmd.none)
    ClipDetails (Ok twitchClip) ->
      ( { model | thanks =
        case model.thanks of
          ThanksClip name clip ->
            ThanksClip name {clip | duration = Just (twitchClip.duration * Time.second)}
          SelfClip clip ->
            SelfClip {clip | duration = Just (twitchClip.duration * Time.second)}
          _ -> model.thanks
        }
      , Cmd.none
      )
    ClipDetails (Err error) ->
      let _ = Debug.log "clip detail fetch error" error in
      (model, Cmd.none)
    Pick (self, selector) ->
      let
        selfClips = Array.filter isSelf model.clips
        otherClips = Array.filter (not<<isSelf) model.clips
        clips = case (self, Array.length selfClips, Array.length otherClips) of
          (_, 0, 0) -> Array.empty
          (_, 0, _) -> otherClips
          (_, _, 0) -> selfClips
          (True, _, _) -> selfClips
          (False, _, _) -> otherClips
        index = floor (selector * (toFloat (Array.length clips)))
        thanks = Array.get index clips
          |> Maybe.withDefault NoHosts
      in
      ( { model
        | thanks = thanks
        , pendingRequests = List.append
          [ case thanks of
              ThanksClip _ clip -> fetchClip clip.id
              SelfClip clip -> fetchClip clip.id
              _ -> Cmd.none
          ]
          model.pendingRequests
        }
      , Cmd.none
      )
    NextChoice time ->
      ( { model
        | pendingRequests = List.append model.pendingRequests
          [ if model.hostLimit >= List.length model.hosts then
              case model.userId of
                Just id -> fetchHosts id
                Nothing -> Cmd.none
            else
              Cmd.none
          ]
        }
      , pickCommand model
      )
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
    WindowSize size ->
      ( { model | windowWidth = size.width, windowHeight = size.height }, Cmd.none)
    UI (View.SetUsername username) ->
      ( { model
        | pendingRequests =
          List.append [fetchUserByName username] model.pendingRequests
        }
      , Cmd.none)

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
          ThanksClip _ {duration} ->
            Time.every
              (duration |> Maybe.withDefault clipCycleTime)
              NextChoice
          SelfClip {duration} ->
            Time.every
              (duration |> Maybe.withDefault clipCycleTime)
              NextChoice
          Thanks _ -> Time.every noClipCycleTime NextChoice
          NoHosts -> Time.every clipCycleTime NextChoice
    , Window.resizes WindowSize
    ]

maybePickCommand : Model -> Cmd Msg
maybePickCommand model =
  if Array.isEmpty model.clips then
    pickCommand model
  else
    Cmd.none

pickCommand : Model -> Cmd Msg
pickCommand model = 
  Random.generate Pick
    <| Random.map2 (,)
      (Random.int 0 (List.length model.hosts)
        |> Random.map (\i -> i == 0)
      )
      (Random.float 0 1)

isSelf : Choice -> Bool
isSelf choice =
  case choice of
    SelfClip _ -> True
    _ -> False

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

fetchClipsUrl : Int -> String -> String
fetchClipsUrl count id =
  "https://api.twitch.tv/helix/clips?broadcaster_id=" ++ id ++ "&first=" ++ (toString count)

fetchClips : Int -> String -> Cmd Msg
fetchClips count id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.clips
    , tagger = Response << Clips id
    , url = (fetchClipsUrl count id)
    }

fetchClipUrl : String -> String
fetchClipUrl slug =
  "https://clips.twitch.tv/api/v2/clips/" ++ slug

fetchClip : String -> Cmd Msg
fetchClip slug =
  Http.send (Response << ClipDetails) <| Http.request
    { method = "GET"
    , headers = []
    , url = fetchClipUrl slug
    , body = Http.emptyBody
    , expect = Http.expectJson ClipsV2.clip
    , timeout = Nothing
    , withCredentials = False
    }

fetchHostsUrl : String -> String
fetchHostsUrl id =
  "https://p3szakkejk.execute-api.us-east-1.amazonaws.com/production/hosts?include_logins=1&target=" ++ id

fetchHosts : String -> Cmd Msg
fetchHosts id =
  Http.send (Response << Hosts) <| Http.request
    { method = "GET"
    , headers = []
    , url = fetchHostsUrl id
    , body = Http.emptyBody
    , expect = Http.expectJson Tmi.hosts
    , timeout = Nothing
    , withCredentials = False
    }

myClip : Helix.Clip -> Clip
myClip clip =
  { id = clip.id
  , url = clip.url
  , embedUrl = clip.embedUrl
  , broadcasterId = clip.broadcasterId
  , duration = Nothing
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

createQueryString : Model -> String
createQueryString model =
  String.join "&"
    [ (case model.userId of
        Just id -> "userId=" ++ id
        Nothing -> (case model.login of
          Just name -> "login=" ++ name
          Nothing -> "")
      )
    , if model.showClip == False then
        "showClip=false"
      else 
        ""
    , if model.hostLimit < requestLimit then
        "hostLimit=" ++ (toString model.hostLimit)
      else
        ""
    ]

createPath : Model -> String
createPath model =
  model.location.pathname ++ "?" ++ (createQueryString model)
