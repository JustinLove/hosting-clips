module HostingClips exposing (..)

import Twitch.Deserialize
import Twitch exposing (helix)
import TwitchId
import View exposing (Clip)

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
  = User (Result Http.Error (List Twitch.Deserialize.User))
  | Response Msg
  | NextRequest Time
  | CurrentUrl Location
  | UI (View.Msg)

type alias Model =
  { location : Location
  , login : Maybe String
  , userId : Maybe String
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
    clips = Json.Decode.decodeString Twitch.Deserialize.clips Twitch.Deserialize.sampleClip |> Result.map (List.map myClip) |> Result.withDefault []
  in
  ( { location = location
    , login = mlogin
    , userId = muserId
    , clips = Dict.singleton "x" clips
    , displayedBroadcaster = Just "x"
    , displayedClip = List.head clips
    , pendingRequests = [
      case muserId of
        Just id -> fetchUserById id
        Nothing ->
          case mlogin of
            Just login -> fetchUserByName login
            Nothing -> Cmd.none
      ]
    , outstandingRequests = 1
    }
  , Cmd.none
  )

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
  helix <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Twitch.Deserialize.users
    , tagger = Response << User
    , url = (fetchUserByNameUrl login)
    }

fetchUserByIdUrl : String -> String
fetchUserByIdUrl id =
  "https://api.twitch.tv/helix/users?id=" ++ id

fetchUserById : String -> Cmd Msg
fetchUserById id =
  helix <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Twitch.Deserialize.users
    , tagger = Response << User
    , url = (fetchUserByIdUrl id)
    }

myClip : Twitch.Deserialize.Clip -> Clip
myClip clip =
  { id = clip.id
  , embedUrl = clip.embedUrl
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
