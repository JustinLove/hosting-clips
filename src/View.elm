module View exposing (Msg(..), Choice(..), Host, view, document)

import Persist exposing (Clip)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode

type Msg
  = SetUsername String
  | Exclude String
  | ShowRecent
  | ShowManage

type Choice
  = ThanksClip String Clip
  | Thanks String
  | SelfClip Clip
  | NoHosts

type alias Host =
  { hostId : String
  , hostDisplayName : String
  }

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
    [ case model.thanks of
        ThanksClip name clip ->
          div [ class "host clip" ]
            [ displayName name
            , if model.showClip then
                displayClip model.windowWidth (model.windowHeight - 75) clip
              else
                text clip.embedUrl
            , displayClipActions clip
            ]
        Thanks name ->
          div [ class "host no-clip" ]
            [ displayName name
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
                displayClip model.windowWidth (model.windowHeight - 75) clip
              else
                text clip.embedUrl
            , displayClipActions clip
            ]
        NoHosts ->
          div [ class "no-hosts" ]
            [ h1 [ class "thanks" ] [ text "Thanks for watching!" ]
            , case model.login of
              Just name -> h2 [ class "host-command" ] [ text ("/host " ++ name) ]
              Nothing ->
                case model.userId of
                  Just _ -> text ""
                  Nothing -> displayNameEntryBox model.login
            ]
    , displayFooter
    , displayActions model
    ]

manageView model = 
  div [ class "view" ]
    [ h2 [] [text "Manage"]
    , model.clipCache
      |> Dict.values
      |> List.concatMap (\(_,clips) -> clips)
      |> List.sortBy (\clip -> clip.id)
      |> List.map (\clip -> li [] [ text clip.id ])
      |> ul []
    , displayFooter
    , displayActions model
    ]

displayName : String -> Html msg
displayName name =
  h1 []
    [ text "Thanks "
    , span [ class "channel-name" ] [ text name ]
    , text " for the host"
    ]

displayClip : Int -> Int -> Clip -> Html msg
displayClip width height clip =
  div []
    [ div
      [ class "clip-url"
      , style "height" ((String.fromFloat ((toFloat height) * 0.1)) ++ "px ")
      , style "margin-bottom" ((String.fromFloat ((toFloat height) * -0.1)) ++ "px ")
      ]
      [ a [ href clip.url ] [ text clip.url ] ]
    , iframe
      [ src clip.embedUrl
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
    ThanksClip name clip ->
      li []
        [ button [ onClick (Exclude clip.id) ]
          [ text ("exclude " ++ name ++ " " ++ clip.id) ]
        ]
    Thanks name -> text ""
    SelfClip clip ->
      li []
        [ button [ onClick (Exclude clip.id) ]
          [ text ("exclude your " ++ clip.id) ]
        ]
    NoHosts -> text ""

displayFooter : Html msg
displayFooter =
  footer []
    [ a [ href "https://github.com/JustinLove/hosting-clips" ]
      [ icon "github", text "hosting-clips" ]
    , text " "
    , a [ href "https://twitter.com/wondible" ]
      [ icon "twitter", text "@wondible" ]
    , text " "
    , a [ href "https://twitch.tv/wondible" ]
      [ icon "twitch", text "wondible" ]
    ]

displayNameEntryBox : Maybe String -> Html Msg
displayNameEntryBox login =
  div [ class "name-entry" ]
    [ label [ for "channelname" ] [ text "Channel Name" ]
    , text " "
    , input
      [ type_ "text"
      , id "channelname"
      , name "channelname"
      , placeholder (Maybe.withDefault "" login)
      , on "change" <| targetValue Json.Decode.string SetUsername
      ] []
    ]

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
