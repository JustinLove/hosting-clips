module View exposing (Msg(..), Choice(..), Clip, Host, view)

import Persist exposing (Clip)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode

type Msg
  = SetUsername String
  | Exclude String

type Choice
  = ThanksClip String Clip
  | Thanks String
  | SelfClip Clip
  | NoHosts

type alias Host =
  { hostId : String
  , hostDisplayName : String
  }

css = """
html, head, body {
  height: 100%;
  margin: 0;
}
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
.view {
  height: 100%;
  position: relative;
}
.host h1 {
  background-color: #2c2541;
  color: rgb(250, 249, 250);
  margin: 0;
  padding: 0.5em;
  text-align: center;
}
.channel-name {
  color: rgb(218, 216, 222);
}
.no-hosts {
  display: flex;
  flex-direction: column;
  justify-content: center;
  height: 100%;
}
.no-hosts .thanks {
  text-align: center;
}
.no-hosts .host-command {
  text-align: center;
}
.no-hosts .name-entry {
  text-align: center;
}
.clip-url {
  text-align: center;
  display: flex;
  flex-direction: column;
  justify-content: center;
}
.actions {
  position: fixed;
  bottom: 0;
  right: 15em;
}
.actions button {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
  border: none;
  color: #8879a5;
}
footer {
  position: fixed;
  bottom: 0;
}
svg.icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  vertical-align: -0.2em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}
.icon-github { color: #888; }
.icon-twitter { color: #55acee; }
.icon-twitch { color: #6441A4; }
a:link, a:visited { color: #b19dd8; }
a:hover, a:active { color: rgb(218, 216, 222); }
"""

view model = 
  div [ class "view" ]
    [ node "style" [] [ text css ]
    , case model.thanks of
        ThanksClip name clip ->
          div [ class "host clip" ]
            [ displayName name
            , if model.showClip then
                displayClip model.windowWidth (model.windowHeight - 75) clip
              else
                text clip.embedUrl
            , displayActions clip
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
            , displayActions clip
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
      , style
        [ ("height", (toString ((toFloat height) * 0.1)) ++ "px ")
        , ("margin-bottom", (toString ((toFloat height) * -0.1)) ++ "px ")
        ]
      ]
      [ a [ href clip.url ] [ text clip.url ] ]
    , iframe
      [ src clip.embedUrl
      , sandbox "allow-scripts allow-same-origin"
      , attribute "allow" "autoplay"
      , attribute "width" (toString ((toFloat width) * 0.8))
      , attribute "height" (toString ((toFloat height) * 0.8))
      , attribute "scrolling" "no"
      , attribute "frameborder" "0"
      , style
        [ ("padding",
            (toString ((toFloat height) * 0.1)) ++ "px " ++
            (toString ((toFloat width) * 0.1)) ++ "px"
          )
        ]
      ] []
    ]

displayActions : Clip -> Html Msg
displayActions clip =
  div [ class "actions" ]
    [ button [ onClick (Exclude clip.id) ] [ text ("exclude " ++ clip.id) ]
    ]

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
    [ use [ xlinkHref ("#icon-"++name) ] [] ]


targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)
