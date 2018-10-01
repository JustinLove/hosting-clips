module HostingClips exposing (..)

import LocalStorage
import Persist exposing (Persist, Clip, DurationInMilliseconds)
import Persist.Encode
import Persist.Decode
import Twitch.Helix.Decode as Helix
import Twitch.Tmi.Decode as Tmi
import Twitch.ClipsV2.Decode as ClipsV2
import Twitch.Helix as Helix
import TwitchId
import View exposing (Choice(..), Host)

import Html
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Http
import Time exposing (Posix)
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Array exposing (Array)
import Random
import Task
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser
import Url.Parser.Query

requestLimit = 100
rateLimit = 30
requestRate = 60*1000/rateLimit
clipCycleTime = 60*1000
noClipCycleTime = 10*1000
selfClipCount = 100
otherClipCount = 20
clipCacheTime = 48 * 60 * 60 * 1000

type Msg
  = Loaded (Maybe Persist)
  | User (Result Http.Error (List Helix.User))
  | Hosts (Result Http.Error (List Tmi.Host))
  | Clips String (Result Http.Error (List Helix.Clip))
  | ClipDetails (Result Http.Error ClipsV2.Clip)
  | Pick (Bool, Float)
  | NextChoice Posix
  | Response Msg
  | NextRequest Posix
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | WindowSize (Int, Int)
  | UI (View.Msg)

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , windowWidth : Int
  , windowHeight : Int
  , time : Posix
  , login : Maybe String
  , userId : Maybe String
  , showClip : Bool
  , selfRate : Float
  , hostLimit : Int
  , hosts : List Host
  , durations : Dict String DurationInMilliseconds
  , clipCache : Dict String (Posix, List Clip)
  , clips : Array Choice
  , thanks : Choice
  , recentClips : List Choice
  , showingRecent : Bool
  , exclusions : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

main = Browser.application
  { init = init
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags location key =
  let
    mlogin = extractSearchArgument "login" location
    muserId = extractSearchArgument "userId" location
    mshowClip = extractSearchArgument "showClip" location
    mselfRate = extractSearchArgument "selfRate" location
    mhostLimit = extractSearchArgument "hostLimit" location
  in
  ( { location = location
    , navigationKey = key
    , windowWidth = 852
    , windowHeight = 480
    , time = Time.millisToPosix 0
    , login = mlogin
    , userId = muserId
    , showClip = case Maybe.withDefault "true" mshowClip of
        "false" -> False
        _ -> True
    , selfRate = mselfRate
      |> Maybe.andThen String.toFloat
      |> Maybe.withDefault 1.0
    , hostLimit = mhostLimit
      |> Maybe.andThen String.toInt
      |> Maybe.withDefault requestLimit
    , hosts = []
    , durations = Dict.empty
    , clipCache = Dict.empty
    , clips = Array.empty
    , thanks = NoHosts
    , recentClips = []
    , showingRecent = False
    , exclusions = []
    , pendingRequests = [] |> appendRequests
      ( case (muserId, mlogin) of
          (Just id, Just login) -> [ fetchHosts id ]
          (Just id, Nothing) -> [ fetchUserById id, fetchHosts id ] 
          (Nothing, Just login) -> [ fetchUserByName login ]
          (Nothing, Nothing) -> [ Cmd.none ]
      )
    , outstandingRequests = 0
    }
  , Dom.getViewport
    |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
    |> Task.perform WindowSize
  )

update msg model =
  case msg of
    Loaded mstate ->
      ( ( case mstate of
          Just state ->
            resolveLoaded { model
              | exclusions = state.exclusions
              , durations = Dict.fromList state.durations
              , clipCache = state.clipCache
              }
          Nothing ->
            model
        )
      , Cmd.none
      )
    User (Ok (user::_)) ->
      let
        m2 =
          { model
          | login = Just user.login
          , userId = Just user.id
          }
      in
      ( m2
      , if (Just user.id) /= model.userId then
          Cmd.batch
            [ Navigation.pushUrl m2.navigationKey (createPath m2)
            , fetchHosts user.id
            , fetchClips selfClipCount user.id
            ]
        else if (Just user.login) /= model.login then
          Cmd.batch
            [ Navigation.pushUrl m2.navigationKey (createPath m2)
            , pickCommand m2
            ]
        else
          pickCommand m2
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
        (cached, new) = hosts
          |> List.take model.hostLimit
          |> List.filter (\host -> List.all ((/=) host) model.hosts)
          |> List.partition (\{hostId} ->
              case Dict.get hostId model.clipCache of
                Just (time, clips) -> (Time.posixToMillis time) < ((Time.posixToMillis model.time) - clipCycleTime)
                Nothing -> False
            )
        requests = new
          |> List.map (\{hostId} -> fetchClips otherClipCount hostId)
        choices = cached
          |> List.concatMap (\{hostId} ->
              case Dict.get hostId model.clipCache of
                Just (time, clips) -> importClips hostId {model|hosts = hosts} clips
                Nothing -> []
              )
          |> Array.fromList
      in
      ( { model
        | hosts = hosts
        , clips = Array.append model.clips choices
        , pendingRequests = model.pendingRequests |> appendRequests requests
        }
      , maybePickCommand model
      )
    Hosts (Err error) ->
      let _ = Debug.log "hosts fetch error" error in
      (model, Cmd.none)
    Clips id (Ok twitchClips) ->
      let
        clips = twitchClips
          |> List.map myClip
        choices = importClips id model clips |> Array.fromList
        m2 =
          { model
          | clips = Array.append model.clips choices
          , clipCache = Dict.insert id (model.time, clips) model.clipCache
          }
      in
      ( m2
      , Cmd.batch [ saveState m2, maybePickCommand model ]
      )
    Clips id (Err error) ->
      let _ = Debug.log "clip fetch error" error in
      (model, Cmd.none)
    ClipDetails (Ok twitchClip) ->
      let
        duration = round (twitchClip.duration * 1000)
        updateChoiceDuration choice =
          case choice of
            ThanksClip name clip ->
              if clip.id == twitchClip.slug then
                ThanksClip name {clip | duration = Just duration}
              else 
                choice
            SelfClip clip ->
              if clip.id == twitchClip.slug then
                SelfClip {clip | duration = Just duration}
              else 
                choice
            _ -> choice
      in
      persist <|
        { model
        | thanks = updateChoiceDuration model.thanks
        , clips = Array.map updateChoiceDuration model.clips
        , durations = Dict.insert twitchClip.slug duration model.durations
        }
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
        , recentClips = thanks :: model.recentClips
        , pendingRequests = model.pendingRequests |> prependRequests
          [ case thanks of
              ThanksClip _ clip ->
                if clip.duration == Nothing then
                  fetchClip clip.id
                else
                  Cmd.none
              SelfClip clip ->
                if clip.duration == Nothing then
                  fetchClip clip.id
                else
                  Cmd.none
              _ -> Cmd.none
          ]
        }
      , Cmd.none
      )
    NextChoice time ->
      ( { model
        | pendingRequests = model.pendingRequests |> appendRequests
          [ if model.hostLimit >= List.length model.hosts then
              case model.userId of
                Just id -> fetchHosts id
                Nothing -> Cmd.none
            else
              Cmd.none
          ]
        , time = time
        }
      , pickCommand model
      )
    Response subMsg ->
      update subMsg { model | outstandingRequests = model.outstandingRequests - 1}
    NextRequest time ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            , time = time
            }, next)
        _ -> ({model | time = time}, Cmd.none)
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)
    WindowSize (width, height) ->
      ( { model | windowWidth = width, windowHeight = height }, Cmd.none)
    UI (View.SetUsername username) ->
      ( { model
        | pendingRequests = model.pendingRequests |> prependRequests
          [fetchUserByName username]
        }
      , Cmd.none)
    UI (View.Exclude id) ->
      let
        m2 =
          { model
          | exclusions = id :: model.exclusions
          , clips = model.clips
            |> Array.filter (\choice ->
              case choice of
                ThanksClip _ clip -> notExcluded model.exclusions clip
                SelfClip clip -> notExcluded model.exclusions clip
                _ -> True
              )
          }
      in
      ( m2
      , Cmd.batch [ pickCommand model, saveState m2 ])
    UI (View.ShowRecent) ->
      ({ model | showingRecent = not model.showingRecent }, Cmd.none)

importClips : String -> Model -> List Clip -> List Choice
importClips id model clips=
  if List.isEmpty clips then
    if (Just id) == model.userId then
      []
    else
      [Thanks (displayNameForHost model.hosts id)]
  else
    clips
      |> List.filter (notExcluded model.exclusions)
      |> List.map (updateClipDuration model.durations)
      |> List.map (\clip ->
          if (Just id) == model.userId then
            SelfClip clip
          else
            ThanksClip (displayNameForHost model.hosts clip.broadcasterId) clip
      )

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist model.exclusions (Dict.toList model.durations) model.clipCache
    |> Persist.Encode.persist
    |> LocalStorage.saveJson

resolveLoaded : Model -> Model
resolveLoaded model =
  let
    choices = case model.userId of
      Just id ->
        case Dict.get id model.clipCache of
          Just (time, clips) -> importClips id model clips
          Nothing -> []
      Nothing -> []
    requests = case model.userId of
      Just id ->
        if Dict.member id model.clipCache then
          []
        else
          [fetchClips selfClipCount id]
      Nothing -> []
  in
  { model
  | clips = Array.append model.clips (Array.fromList choices)
  , pendingRequests = model.pendingRequests |> appendRequests requests
  }

appendRequests : List (Cmd msg) -> List (Cmd msg) -> List (Cmd msg)
appendRequests requests pendingRequests =
  List.filter ((/=) Cmd.none) <| List.append pendingRequests requests

prependRequests : List (Cmd msg) -> List (Cmd msg) -> List (Cmd msg)
prependRequests requests pendingRequests =
  List.filter ((/=) Cmd.none) <| List.append requests pendingRequests

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
              (duration |> Maybe.withDefault clipCycleTime |> toFloat)
              NextChoice
          SelfClip {duration} ->
            Time.every
              (duration |> Maybe.withDefault clipCycleTime |> toFloat)
              NextChoice
          Thanks _ -> Time.every noClipCycleTime NextChoice
          NoHosts -> Time.every clipCycleTime NextChoice
    , LocalStorage.loadedJson Persist.Decode.persist Loaded
    , Browser.Events.onResize (\w h -> WindowSize (w, h))
    ]

maybePickCommand : Model -> Cmd Msg
maybePickCommand model =
  if Array.isEmpty model.clips || model.thanks == NoHosts then
    pickCommand model
  else
    Cmd.none

pickCommand : Model -> Cmd Msg
pickCommand model = 
  Random.generate Pick
    <| Random.map2 Tuple.pair
      (Random.float 0 ((List.length model.hosts |> toFloat) / model.selfRate)
        |> Random.map (\i -> i < 1)
      )
      (Random.float 0 1)

isSelf : Choice -> Bool
isSelf choice =
  case choice of
    SelfClip _ -> True
    _ -> False

notExcluded : List String -> Clip -> Bool
notExcluded exclusions clip =
  not <| List.any (\ex -> clip.id == ex) exclusions

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
  "https://api.twitch.tv/helix/clips?broadcaster_id=" ++ id ++ "&first=" ++ (String.fromInt count)

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

updateClipDuration : Dict String DurationInMilliseconds -> Clip -> Clip
updateClipDuration durations clip =
  { clip | duration = Dict.get clip.id durations }

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

extractSearchArgument : String -> Url -> Maybe String
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing

createQueryString : Model -> List Url.QueryParameter
createQueryString model =
  [ Maybe.map (Url.string "userId") model.userId
  , Maybe.map (Url.string "login") model.login
  , if model.showClip == False then
      Just <| Url.string "showClip" "false"
    else 
      Nothing
  , if model.selfRate /= 0.0 then
      Just <| Url.string "selfRate" (String.fromFloat model.selfRate)
    else
      Nothing
  , if model.hostLimit < requestLimit then
      Just <| Url.int "hostLimit" model.hostLimit
    else
    Nothing
  ]
    |> List.filterMap identity

createPath : Model -> String
createPath model =
  Url.relative [] (createQueryString model)
