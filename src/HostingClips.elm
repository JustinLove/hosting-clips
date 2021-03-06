module HostingClips exposing (..)

import LocalStorage
import Persist exposing (Persist, Clip, DurationInMilliseconds)
import Persist.Encode
import Persist.Decode
import Twitch.Helix.Decode as Helix
import Twitch.Helix as Helix
import Twitch.Kraken as Kraken
import Twitch.Kraken.Decode as Kraken
import TwitchId
import View exposing (Choice(..), UserId, ClipId)

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
import Set exposing (Set)
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
  | HttpError String Http.Error
  | User (List Helix.User)
  | Broadcaster (List Helix.User)
  | Hosts (List Kraken.Host)
  | Clips UserId (List Helix.Clip)
  | Pick (Bool, Float)
  | NextChoice Posix
  | Response Msg
  | NextRequest Posix
  | CurrentTime Posix
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
  , userId : Maybe UserId
  , auth : Maybe String
  , showClip : Bool
  , selfRate : Float
  , hostLimit : Int
  , hosts : List UserId
  , userDisplayNames : Dict UserId String
  , durations : Dict ClipId DurationInMilliseconds
  , clipCache : Dict UserId (Posix, List Clip)
  , clips : Array Choice
  , thanks : Choice
  , exclusions : Set ClipId
  , recentClips : List Choice
  , showingRecent : Bool
  , showingManage : Bool
  , clipFilter : String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

type alias Host =
  { hostId : UserId
  , hostDisplayName : String
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
    (initialModel, initialCmd) =
      { location = location
      , navigationKey = key
      , windowWidth = 852
      , windowHeight = 480
      , time = Time.millisToPosix 0
      , login = Nothing
      , userId = Nothing
      , auth = Nothing
      , showClip = True
      , selfRate = 1.0
      , hostLimit = requestLimit
      , hosts = []
      , userDisplayNames = Dict.empty
      , durations = Dict.empty
      , clipCache = Dict.empty
      , clips = Array.empty
      , thanks = NoHosts
      , exclusions = Set.empty
      , recentClips = []
      , showingRecent = False
      , showingManage = False
      , clipFilter = ""
      , pendingRequests = []
      , outstandingRequests = 0
      }
        |> update (CurrentUrl location)
  in
  ( initialModel
  , Cmd.batch
    [ initialCmd
    , Dom.getViewport
      |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      |> Task.perform WindowSize
    , Time.now |> Task.perform CurrentTime
    ]
  )

logout : Model -> Model
logout model =
  { location = model.location
  , navigationKey = model.navigationKey
  , windowWidth = model.windowWidth
  , windowHeight = model.windowHeight
  , time = model.time
  , login = Nothing
  , userId = Nothing
  , auth = Nothing
  , showClip = model.showClip
  , selfRate = model.selfRate
  , hostLimit = model.hostLimit
  , hosts = []
  , userDisplayNames = Dict.empty
  , durations = Dict.empty
  , clipCache = Dict.empty
  , clips = Array.empty
  , thanks = NoHosts
  , exclusions = Set.empty
  , recentClips = []
  , showingRecent = False
  , showingManage = False
  , clipFilter = ""
  , pendingRequests = []
  , outstandingRequests = model.outstandingRequests
  }

update msg model =
  case msg of
    Loaded mstate ->
      ( ( case mstate of
          Just state ->
            resolveLoaded { model
              | exclusions = Set.fromList state.exclusions
              , durations = Dict.fromList state.durations
              , clipCache = state.clipCache
              }
          Nothing ->
            model
        )
      , Cmd.none
      )
    HttpError source (Http.BadStatus 401) ->
      let _ = Debug.log ("fetch auth error: " ++ source) "" in
      (logout model, Cmd.none)
    HttpError "hosts" (error) ->
      let _ = Debug.log ("fetch error: hosts") error in
      (model, maybePickCommand model)
    HttpError source (error) ->
      let _ = Debug.log ("fetch error: " ++ source) error in
      (model, Cmd.none)
    User (user::_) ->
      let
        m2 =
          { model
          | login = Just user.login
          , userId = Just user.id
          }
      in
      ( m2
      , if (Just user.id) /= model.userId then
          case model.auth of
            Just auth ->
              Cmd.batch
                [ Navigation.pushUrl m2.navigationKey (createPath m2)
                , fetchHosts auth user.id
                , fetchClips auth selfClipCount user.id
                ]
            Nothing ->
                Navigation.pushUrl m2.navigationKey (createPath m2)
        else if (Just user.login) /= model.login then
          Cmd.batch
            [ Navigation.pushUrl m2.navigationKey (createPath m2)
            , pickCommand m2
            ]
        else
          pickCommand m2
      )
    User _ ->
      let _ = Debug.log "user did not find that login name" "" in
      (model, Cmd.none)
    Broadcaster (user::_) ->
      let _ = Debug.log "boadcaster" user in
      ( { model
        | userDisplayNames = Dict.insert user.id user.displayName model.userDisplayNames
        , clips = Array.map (updateName user.id user.displayName) model.clips
        , recentClips = List.map (updateName user.id user.displayName) model.recentClips
        , thanks = updateName user.id user.displayName model.thanks
        }
      , Cmd.none
      )
    Broadcaster _ ->
      let _ = Debug.log "broadcaster did not find that login name" "" in
      (model, Cmd.none)
    Hosts twitchHosts ->
      let
        hosts = List.map myHost twitchHosts
        (new, cached) = hosts
          |> List.take model.hostLimit
          |> List.filter (\host -> List.all ((/=) host) model.hosts)
          |> List.partition (\hostId ->
              case Dict.get hostId model.clipCache of
                Just (time, clips) ->
                  (Time.posixToMillis time) < ((Time.posixToMillis model.time) - clipCacheTime)
                Nothing -> False
            )
        requests = case model.auth of
          Just auth ->
            new
              |> List.map (\hostId -> fetchClips auth otherClipCount hostId)
          Nothing ->
            []
        choices = cached
          |> List.concatMap (\hostId ->
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
    Clips id twitchClips ->
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
          [ requestVideoDataForThanks thanks model
          , requestNameForThanks thanks model
          ]
        }
      , Cmd.none
      )
    NextChoice time ->
      ( { model
        | pendingRequests = model.pendingRequests |> appendRequests
          [ if model.hostLimit >= List.length model.hosts then
              case (model.userId, model.auth) of
                (Just id, Just auth) -> fetchHosts auth id
                _ -> Cmd.none
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
    CurrentTime time ->
      ({model | time = time}, Cmd.none)
    CurrentUrl location ->
      let
        mlogin = extractSearchArgument "login" location
        muserId = extractSearchArgument "userId" location
        mshowClip = extractSearchArgument "showClip" location
        mselfRate = extractSearchArgument "selfRate" location
        mhostLimit = extractSearchArgument "hostLimit" location
        mauth = extractHashArgument "access_token" location
      in
        { model
        | location = location
        , login = mlogin
        , userId = muserId
        , auth = mauth
        , showClip = case Maybe.withDefault "true" mshowClip of
            "false" -> False
            _ -> True
        , selfRate = mselfRate
          |> Maybe.andThen String.toFloat
          |> Maybe.withDefault 1.0
        , hostLimit = mhostLimit
          |> Maybe.andThen String.toInt
          |> Maybe.withDefault requestLimit
        , pendingRequests = model.pendingRequests |> appendRequests
          ( case mauth of
              Just auth ->
                ( case (muserId, mlogin) of
                  (Just id, Just login) -> [ fetchHosts auth id ]
                  (Just id, Nothing) -> [ fetchUserById auth id, fetchHosts auth id ] 
                  (Nothing, Just login) -> [ fetchUserByName auth login ]
                  (Nothing, Nothing) -> [ fetchSelf auth ]
                )
              Nothing ->
                [ Cmd.none ]
          )
        }
          |> update (NextRequest model.time)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)
    WindowSize (width, height) ->
      ( { model | windowWidth = width, windowHeight = height }, Cmd.none)
    UI (View.Exclude id) ->
      if Set.member id model.exclusions then
        { model
        | exclusions = Set.remove id model.exclusions
        }
          |> persist
      else
        let
          m2 =
            { model
            | exclusions = Set.insert id model.exclusions
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
    UI (View.ShowManage) ->
      ({ model | showingManage = not model.showingManage }, Cmd.none)
    UI (View.ClipFilter query) ->
      ( { model | clipFilter = query }
      , Cmd.none)
    UI (View.ClipDuration id seconds) ->
      --let _ = Debug.log id seconds in
      let
        duration = round (seconds * 1000)
        updateChoiceDuration choice =
          case choice of
            ThanksClip mname clip ->
              if clip.id == id then
                ThanksClip mname {clip | duration = Just duration}
              else 
                choice
            SelfClip clip ->
              if clip.id == id then
                SelfClip {clip | duration = Just duration}
              else 
                choice
            _ -> choice
      in
      persist <|
        { model
        | thanks = updateChoiceDuration model.thanks
        , clips = Array.map updateChoiceDuration model.clips
        , durations = Dict.insert id duration model.durations
        }

importClips : UserId -> Model -> List Clip -> List Choice
importClips id model clips=
  if List.isEmpty clips then
    if (Just id) == model.userId then
      []
    else
      [Thanks (displayNameForHost model.userDisplayNames id |> Maybe.withDefault id)]
  else
    clips
      |> List.filter (notExcluded model.exclusions)
      |> List.map (updateClipDuration model.durations)
      |> List.map (\clip ->
          if (Just id) == model.userId then
            SelfClip clip
          else
            ThanksClip (displayNameForHost model.userDisplayNames clip.broadcasterId) clip
      )

updateName : UserId -> String -> Choice -> Choice
updateName id name choice =
  case choice of
    ThanksClip _ clip ->
      if clip.broadcasterId == id then
        ThanksClip (Just name) clip
      else
        choice
    _ ->
      choice

requestVideoDataForThanks : Choice -> Model -> Cmd Msg
requestVideoDataForThanks thanks model =
  case (thanks, model.auth) of
    (ThanksClip _ clip, Just auth) ->
      (case (clip.duration, clip.videoUrl) of
        (Nothing, Nothing) ->
          let _ = Debug.log "backfill video url" clip.id in
          fetchClips auth otherClipCount clip.broadcasterId
        _ -> Cmd.none
      )
    (SelfClip clip, Just auth) ->
      (case (clip.duration, clip.videoUrl) of
        (Nothing, Nothing) ->
          let _ = Debug.log "backfill video url" clip.id in
          fetchClips auth selfClipCount clip.broadcasterId
        _ -> Cmd.none
      )
    _ -> Cmd.none

requestNameForThanks : Choice -> Model -> Cmd Msg
requestNameForThanks thanks model =
  case (thanks, model.auth) of
    (ThanksClip _ clip, Just auth) ->
      (case Dict.get clip.broadcasterId model.userDisplayNames of
        Nothing ->
          let _ = Debug.log "backfill user name" clip.broadcasterId in
          fetchBroadcasterById auth clip.broadcasterId
        Just _ ->
          Cmd.none
      )
    _ -> Cmd.none

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist
    (Set.toList model.exclusions)
    (Dict.toList model.durations)
    model.clipCache
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
    requests = case (model.auth, model.userId) of
      (Just auth, Just id) ->
        if Dict.member id model.clipCache then
          []
        else
          [fetchClips auth selfClipCount id]
      _ -> []
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

notExcluded : Set ClipId -> Clip -> Bool
notExcluded exclusions clip =
  not <| Set.member clip.id exclusions

httpResponse : String -> (a -> Msg)-> Result Http.Error a -> Msg
httpResponse source success result =
  case result of
    Ok value -> Response (success value)
    Err err -> Response (HttpError source err)

fetchUserByNameUrl : String -> String
fetchUserByNameUrl login =
  "https://api.twitch.tv/helix/users?login=" ++ login

fetchUserByName : String -> String -> Cmd Msg
fetchUserByName auth login =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.users
    , tagger = httpResponse "user by name" User
    , url = (fetchUserByNameUrl login)
    }

fetchUserByIdUrl : UserId -> String
fetchUserByIdUrl id =
  "https://api.twitch.tv/helix/users?id=" ++ id

fetchUserById : String -> UserId -> Cmd Msg
fetchUserById auth id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.users
    , tagger = httpResponse "user by id" User
    , url = (fetchUserByIdUrl id)
    }

fetchBroadcasterById : String -> UserId -> Cmd Msg
fetchBroadcasterById auth id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.users
    , tagger = httpResponse "broadcaster by id" Broadcaster
    , url = (fetchUserByIdUrl id)
    }

fetchSelfUrl : String
fetchSelfUrl =
  "https://api.twitch.tv/helix/users"

fetchSelf : String -> Cmd Msg
fetchSelf auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.users
    , tagger = httpResponse "self" User
    , url = fetchSelfUrl
    }

fetchClipsUrl : Int -> UserId -> String
fetchClipsUrl count id =
  "https://api.twitch.tv/helix/clips?broadcaster_id=" ++ id ++ "&first=" ++ (String.fromInt count)

fetchClips : String -> Int -> UserId -> Cmd Msg
fetchClips auth count id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.clips
    , tagger = httpResponse "clips" (Clips id)
    , url = (fetchClipsUrl count id)
    }

fetchHostsUrl : UserId -> String
fetchHostsUrl id =
  "https://api.twitch.tv/kraken/channels/"++id++"/hosts"

fetchHosts : String -> UserId -> Cmd Msg
fetchHosts auth id =
  Kraken.send <|
    { clientId = TwitchId.clientId
    , auth = Just auth
    , decoder = Kraken.hosts
    , tagger = httpResponse "clips" Hosts
    , url = (fetchHostsUrl id)
    }

myClip : Helix.Clip -> Clip
myClip clip =
  { id = clip.id
  , url = clip.url
  , embedUrl = clip.embedUrl
  , broadcasterId = clip.broadcasterId
  , duration = Nothing
  , videoUrl = clip.thumbnailUrl
    --|> Debug.log "thumnail"
    |> String.replace "-preview-480x272.jpg" ".mp4"
    |> Just
    --|> Debug.log "video url"
  }

updateClipDuration : Dict ClipId DurationInMilliseconds -> Clip -> Clip
updateClipDuration durations clip =
  { clip | duration = Dict.get clip.id durations }

myHost : Kraken.Host -> UserId
myHost host = host.hostId

displayNameForHost : Dict UserId String -> UserId -> Maybe String
displayNameForHost names id =
  Dict.get id names

extractSearchArgument : String -> Url -> Maybe String
extractSearchArgument key location =
  { location | path = "" }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing

extractHashArgument : String -> Url -> Maybe String
extractHashArgument key location =
  { location | path = "", query = location.fragment }
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
  Url.custom Url.Relative [] (createQueryString model) (Maybe.map (\token -> "access_token=" ++ token) model.auth)
