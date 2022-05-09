module HostingClips exposing (..)

import Decode exposing (User)
import LocalStorage
import Log
import Persist exposing (Persist, Clip, DurationInMilliseconds, UserId, ClipId)
import Persist.Encode
import Persist.Decode
import Twitch.Helix.Request as Helix
import TwitchId
import Pagination as Helix
import View exposing (Choice(..))

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
rateLimit = 800
requestRate = 60*1000/rateLimit
clipCycleTime = 60*1000
noClipCycleTime = 10*1000
selfClipCount = 100
otherClipCount = 20
clipCacheTime = 48 * 60 * 60 * 1000
nameCacheTime = 30 * 24 * 60 * 60 * 1000

type WhichRequest
  = FirstRequest
  | OtherRequest String

type Msg
  = Loaded (LocalStorage.LoadResult Persist)
  | HttpError String Http.Error
  | Self (List User)
  | Broadcaster (List User)
  | Clips UserId WhichRequest (Helix.Paginated (List Clip))
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
  , hosts : List UserId
  , userDisplayNames : Dict UserId (Posix, String)
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
    Loaded (LocalStorage.Data state) ->
      ( resolveLoaded { model
          | exclusions = Set.fromList state.exclusions
          , durations = Dict.fromList state.durations
          , clipCache = state.clipCache
          , userDisplayNames = state.nameCache
          }
      , Cmd.none
      )
    Loaded (LocalStorage.Error err) ->
      ( resolveLoaded model
      , Log.decodeError "error loading saved state" err
      )
    Loaded (LocalStorage.NoData) ->
      ( resolveLoaded model
      , Cmd.none
      )
    HttpError source (Http.BadStatus 401) ->
      (logout model, Log.warn ("fetch auth error: " ++ source))
    HttpError "hosts" (error) ->
      (model, Cmd.batch
        [ maybePickCommand model
        , Log.httpError "fetch error: hosts" error
        ]
      )
    HttpError source (error) ->
      (model, Log.httpError ("fetch error: " ++ source) error)
    Self (user::_) ->
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
                , fetchClips auth selfClipCount user.id FirstRequest
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
    Self _ ->
      (model, Log.warn "user did not find that login name")
    Broadcaster (user::_) ->
      { model
      | userDisplayNames = Dict.insert user.id (model.time, user.displayName) model.userDisplayNames
      }
        |> persist
    Broadcaster _ ->
      (model, Log.warn "broadcaster did not find that login name")
    Clips id which (Helix.Paginated mcursor clips) ->
      let
        cache =
          (case (which, Dict.get id model.clipCache ) of
            (FirstRequest, _) ->
              clips
            (OtherRequest _, Just (t, c)) ->
              List.append c clips
            (OtherRequest _, Nothing) ->
              clips
          )
            |> mostRecent selfClipCount
        choices = cache
          |> importClips id model
          |> Array.fromList
        m2 =
          { model
          -- no longer handles multiple channels
          | clips = choices
          , clipCache = Dict.update id (\mx ->
            case (which, mx) of
              (FirstRequest, _) ->
                Just (model.time, cache)
              (OtherRequest _, Just (t, c)) ->
                Just (t, cache)
              (OtherRequest _, Nothing) ->
                Just (model.time, cache)
            )
            model.clipCache
          , pendingRequests = model.pendingRequests |> appendRequests
            (case (model.auth, clips, mcursor) of
              (Just auth, [], _) -> [fetchBroadcasterById auth id]
              (Just auth, _, Just cursor) ->
                if (List.length clips) == selfClipCount then
                  [fetchClips auth selfClipCount id (OtherRequest cursor)]
                else
                  []
              _ -> []
            )
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
      ( { model | time = time }
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
        , pendingRequests = model.pendingRequests |> appendRequests
          ( case mauth of
              Just auth ->
                ( case (muserId, mlogin) of
                  (Just id, Just login) -> []
                  (Just id, Nothing) -> [ fetchUserById auth id ]
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
                  ThanksClip clip -> clipNotExcluded model.exclusions clip
                  SelfClip clip -> clipNotExcluded model.exclusions clip
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
      let
        duration = round (seconds * 1000)
        updateChoiceDuration choice =
          case choice of
            ThanksClip clip ->
              if clip.id == id then
                ThanksClip {clip | duration = Just duration}
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
      [Thanks id]
  else
    clips
      |> List.filter (clipNotExcluded model.exclusions)
      |> List.map (updateClipDuration model.durations)
      |> List.map (\clip ->
          if (Just id) == model.userId then
            SelfClip clip
          else
            ThanksClip clip
      )

appendNewChoices : Array Choice -> Array Choice -> Array Choice
appendNewChoices older newer =
  Array.append
    (Array.filter (choiceNotExcluded (choiceClipIds newer)) older)
    newer

choiceClipIds : Array Choice -> Set ClipId
choiceClipIds choices =
  List.filterMap choiceId (Array.toList choices)
    |> Set.fromList

choiceId : Choice -> Maybe ClipId
choiceId choice =
  case choice of
    ThanksClip clip -> Just clip.id
    Thanks _ -> Nothing
    SelfClip clip -> Just clip.id
    NoHosts -> Nothing

mostRecent : Int -> List Clip -> List Clip
mostRecent count clips =
  clips
    |> List.sortBy (\clip ->
      case clip.createdAt of
        Just t -> Time.posixToMillis t
        Nothing -> 0
      )
    |> List.reverse
    |> List.take count

requestVideoDataForThanks : Choice -> Model -> Cmd Msg
requestVideoDataForThanks thanks model =
  case (thanks, model.auth) of
    (ThanksClip clip, Just auth) ->
      (case (clip.duration, clip.videoUrl) of
        (Nothing, Nothing) ->
          Cmd.batch
            [ Log.info ("backfill video url" ++ clip.id)
            , fetchClips auth otherClipCount clip.broadcasterId FirstRequest
            ]
        _ -> Cmd.none
      )
    (SelfClip clip, Just auth) ->
      (case (clip.duration, clip.videoUrl) of
        (Nothing, Nothing) ->
          Cmd.batch
            [ Log.info ("backfill video url" ++ clip.id)
            , fetchClips auth selfClipCount clip.broadcasterId FirstRequest
            ]
        _ -> Cmd.none
      )
    _ -> Cmd.none

requestNameForThanks : Choice -> Model -> Cmd Msg
requestNameForThanks thanks model =
  case (thanks, model.auth) of
    (ThanksClip clip, Just auth) ->
      (case Dict.get clip.broadcasterId model.userDisplayNames of
        Nothing ->
          Cmd.batch
            [ Log.info ("backfill user name" ++ clip.broadcasterId)
            , fetchBroadcasterById auth clip.broadcasterId
            ]
        Just (time, _) ->
          if (Time.posixToMillis time) < ((Time.posixToMillis model.time) - nameCacheTime) then
            fetchBroadcasterById auth clip.broadcasterId
          else
            Cmd.none
      )
    (Thanks userId, Just auth) ->
      (case Dict.get userId model.userDisplayNames of
        Nothing ->
          Cmd.batch
            [ Log.info ("backfill user name" ++ userId)
            , fetchBroadcasterById auth userId
            ]
        Just (time, _) ->
          if (Time.posixToMillis time) < ((Time.posixToMillis model.time) - nameCacheTime) then
            fetchBroadcasterById auth userId
          else
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
    model.userDisplayNames
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
        case Dict.get id model.clipCache of
          Just (time, _) ->
            if (Time.posixToMillis time) + clipCacheTime > (Time.posixToMillis model.time) then
              [maybePickCommand model]
            else
              [fetchClips auth selfClipCount id FirstRequest]
          Nothing ->
            [fetchClips auth selfClipCount id FirstRequest]
      _ -> []
  in
  { model
  | clips = appendNewChoices (Array.fromList choices) model.clips
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
          ThanksClip {duration} ->
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

clipNotExcluded : Set ClipId -> Clip -> Bool
clipNotExcluded exclusions clip =
  not <| Set.member clip.id exclusions

choiceNotExcluded : Set ClipId -> Choice -> Bool
choiceNotExcluded exclusions choice =
  case choice of
    SelfClip clip -> clipNotExcluded exclusions clip
    ThanksClip clip -> clipNotExcluded exclusions clip
    _ -> True

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
    , decoder = Decode.users
    , tagger = httpResponse "user by name" Self
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
    , decoder = Decode.users
    , tagger = httpResponse "user by id" Self
    , url = (fetchUserByIdUrl id)
    }

fetchBroadcasterById : String -> UserId -> Cmd Msg
fetchBroadcasterById auth id =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Decode.users
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
    , decoder = Decode.users
    , tagger = httpResponse "self" Self
    , url = fetchSelfUrl
    }

fetchClipsUrl : Int -> UserId -> WhichRequest -> String
fetchClipsUrl count id which =
  let
    after = case which of
      FirstRequest -> ""
      OtherRequest cursor -> "&after=" ++ cursor
  in
  "https://api.twitch.tv/helix/clips?broadcaster_id=" ++ id ++ "&first=" ++ (String.fromInt count) ++ after

fetchClips : String -> Int -> UserId -> WhichRequest -> Cmd Msg
fetchClips auth count id which =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.paginated Decode.clips
    , tagger = httpResponse "clips" (Clips id which)
    , url = (fetchClipsUrl count id which)
    }

updateClipDuration : Dict ClipId DurationInMilliseconds -> Clip -> Clip
updateClipDuration durations clip =
  case clip.duration of
    Just _ -> clip
    Nothing -> { clip | duration = Dict.get clip.id durations }

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
  ]
    |> List.filterMap identity

createPath : Model -> String
createPath model =
  Url.custom Url.Relative [] (createQueryString model) (Maybe.map (\token -> "access_token=" ++ token) model.auth)
