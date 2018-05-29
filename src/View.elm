module View exposing (Msg(..), Choice(..), Clip, Host, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)

type Msg
  = None

type Choice
  = ThanksClip String Clip
  | Thanks String
  | NoHosts

type alias Clip =
  { id : String
  , embedUrl : String
  , broadcasterId : String
  }

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
                displayClip model.windowWidth (model.windowHeight - 100) clip
              else
                text clip.embedUrl
            ]
        Thanks name ->
          div [ class "host no-clip" ]
            [ displayName name
            ]
        NoHosts ->
          div [ class "no-hosts" ]
            [ h1 [ class "thanks" ] [ text "Thanks for watching!" ]
            , case model.login of
              Just name -> h2 [ class "host-command" ] [ text ("/host " ++ name) ]
              Nothing -> text ""
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
  iframe
    [ src clip.embedUrl
    , sandbox "allow-scripts allow-same-origin"
    , attribute "allow" "autoplay"
    , attribute "width" (toString width)
    , attribute "height" (toString height)
    , attribute "scrolling" "no"
    , attribute "frameborder" "0"
    ] []

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

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("#icon-"++name) ] [] ]
