module View exposing (Msg(..), Choice(..), UserId, ClipId, view, document)

import Persist exposing (Clip)
import TwitchId

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onCheck, onInput)
import Set
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode
import Url exposing (Url)
import Url.Builder as Url

type Msg
  = Exclude ClipId
  | ShowRecent
  | ShowManage
  | ClipFilter String
  | ClipDuration ClipId Float

type Choice
  = ThanksClip (Maybe String) Clip
  | Thanks UserId
  | SelfClip Clip
  | NoHosts

type alias UserId = String
type alias ClipId = String

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
"""

document tagger model =
  { title = "Hosting Clips"
  , body = [Html.map tagger (view model)]
  }

view model =
  if model.showingManage then
    manageView model
  else
    clipsView model

clipsView model = 
  div [ class "view" ]
    [ node "style" [] [ text css ]
    , case model.thanks of
        ThanksClip mname clip ->
          div [ class "host clip" ]
            [ Maybe.map displayName mname
              |> Maybe.withDefault displayNameless
            , if model.showClip then
                displayClip model.location.host model.windowWidth (model.windowHeight - 75) clip
              else
                text clip.embedUrl
            , displayClipActions clip
            , measureClipDuration clip
            ]
        Thanks userId ->
          div [ class "host no-clip" ]
            [ displayNameForId model userId
            ]
        SelfClip clip ->
          div [ class "self host clip" ]
            [ h1 []
              [ text "You are watching "
              , span [ class "channel-name" ]
                [ text (model.login |> Maybe.withDefault "somebody...?")
                ]
              ]
            , if model.showClip then
                displayClip model.location.host model.windowWidth (model.windowHeight - 75) clip
              else
                text clip.embedUrl
            , displayClipActions clip
            , measureClipDuration clip
            ]
        NoHosts ->
          div [ class "no-hosts" ]
            [ h1 [ class "thanks" ] [ text "Thanks for watching!" ]
            , case model.login of
              Just name -> h2 [ class "host-command" ] [ text ("/host " ++ name) ]
              Nothing ->
                case model.auth of
                  Just _ -> text ""
                  Nothing -> div [ class "login" ] [ displayLogin model ]
            ]
    , displayFooter model
    , displayActions model
    ]

manageView model = 
  div [ class "view" ]
    [ h2 []
      [ text "Manage "
      , input
        [ type_ "text"
        , id "clipfilter"
        , name "clipfilter"
        , value model.clipFilter
        , placeholder "filter"
        , onInput ClipFilter
        ] []
      ]
    , model.clipCache
      |> Dict.values
      |> List.concatMap (\(_,clips) -> clips)
      |> List.filter (\clip -> clip.id |> String.contains model.clipFilter)
      |> List.sortBy (\clip -> clip.id)
      |> List.map (\clip ->
        let
          tag = "exclude-"++clip.id
          excluded = Set.member clip.id model.exclusions
        in
        li [ classList [ ("excluded", excluded) ] ]
          [ input
            [ type_ "checkbox"
            , Html.Attributes.name tag
            , id tag
            , value "selected"
            , onCheck (\_ -> Exclude clip.id)
            , checked excluded
            ] []
          , label [ for tag ] [ text clip.id ]
          ]
        )
      |> ul []
    , displayFooter model
    , displayActions model
    ]

displayName : String -> Html msg
displayName name =
  h1 []
    [ text "Thanks "
    , span [ class "channel-name" ] [ text name ]
    , text " for the host"
    ]

displayNameless : Html msg
displayNameless =
  h1 []
    [ text "Thanks for the host"
    ]

displayNameForId model id =
  case Dict.get id model.userDisplayNames of
    Just name -> displayName name
    Nothing -> displayNameless

displayClip : UserId -> Int -> Int -> Clip -> Html msg
displayClip host width height clip =
  div []
    [ div
      [ class "clip-url"
      , style "height" ((String.fromFloat ((toFloat height) * 0.1)) ++ "px ")
      , style "margin-bottom" ((String.fromFloat ((toFloat height) * -0.1)) ++ "px ")
      ]
      [ a [ href clip.url ] [ text clip.url ] ]
    , iframe
      [ src (clip.embedUrl ++ "&autoplay=true&parent=" ++ host)
      , sandbox "allow-scripts allow-same-origin"
      , attribute "allow" "autoplay"
      , attribute "width" (String.fromFloat ((toFloat width) * 0.8))
      , attribute "height" (String.fromFloat ((toFloat height) * 0.8))
      , attribute "scrolling" "no"
      , attribute "frameborder" "0"
      , style
          "padding"
          ((String.fromFloat ((toFloat height) * 0.1)) ++ "px " ++
          (String.fromFloat ((toFloat width) * 0.1)) ++ "px")
      ] []
    ]

measureClipDuration : Clip -> Html Msg
measureClipDuration clip =
  case (clip.duration, clip.videoUrl) of
    (Nothing, Just videoUrl) -> 
      video
        [ id "duration-probe"
        , src videoUrl
        , preload "metadata"
        , on "loadedmetadata" (targetDuration (ClipDuration clip.id))
        ] []
    _ ->
      text ""

displayClipActions : Clip -> Html Msg
displayClipActions clip =
  div [ class "clip-actions" ]
    [ button [ onClick (Exclude clip.id) ] [ text ("exclude " ++ clip.id) ]
    ]

displayActions model =
  div [ class "actions" ]
    [ button [ onClick (ShowRecent) ] [ text "recent" ]
    , button [ onClick (ShowManage) ] [ text "manage" ]
    , if model.showingRecent then
        model.recentClips
          |> List.reverse
          |> List.map displayRecentClip
          |> ul [ class "recent-clips" ]
      else
        text ""
    ]

displayRecentClip : Choice -> Html Msg
displayRecentClip choice =
  case choice of
    ThanksClip mname clip ->
      li []
        [ button [ onClick (Exclude clip.id) ]
          [ text ("exclude " ++ (Maybe.withDefault "" mname) ++ " " ++ clip.id) ]
        ]
    Thanks id -> text ""
    SelfClip clip ->
      li []
        [ button [ onClick (Exclude clip.id) ]
          [ text ("exclude your " ++ clip.id) ]
        ]
    NoHosts -> text ""

authorizeUrl : String -> String
authorizeUrl redirectUri =
  "https://id.twitch.tv/oauth2/authorize"
    ++ (
      [ Url.string "client_id" TwitchId.clientId
      , Url.string "redirect_uri" redirectUri
      , Url.string "response_type" "token"
      , Url.string "scope" ""
      ]
      |> Url.toQuery
      )

urlForRedirect : Url -> String
urlForRedirect url =
  {url | query = Nothing, fragment = Nothing } |> Url.toString

displayLogin model =
  case model.auth of
    Just _ ->
      span [ ]
        [ text " "
        , text <| Maybe.withDefault "--" model.login
        , text " "
        , a [ href (Url.relative [] []) ]
            [ text "logout" ]
        ]
    Nothing ->
      span []
        [ text " "
        , a [ href (authorizeUrl (urlForRedirect model.location)) ]
            [ icon "twitch", text "login" ]
        ]

-- displayFooter : Model -> Html msg
displayFooter model =
  footer []
    [ a [ href "https://github.com/JustinLove/hosting-clips" ]
      [ icon "github", text "hosting-clips" ]
    , text " "
    , a [ href "https://twitter.com/wondible" ]
      [ icon "twitter", text "@wondible" ]
    , text " "
    , a [ href "https://twitch.tv/wondible" ]
      [ icon "twitch", text "wondible" ]
    , case model.auth of
      Just _ ->
        displayLogin model
      Nothing ->
        text ""
    ]

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]

targetDuration : (Float -> Msg) -> Json.Decode.Decoder Msg
targetDuration tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "duration" ] Json.Decode.float)
