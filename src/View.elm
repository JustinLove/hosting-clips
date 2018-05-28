module View exposing (Msg(..), Choice(..), Clip, Host, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Time exposing (Time)

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
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
h1 {
  background-color: #2c2541;
  color: rgb(250, 249, 250);
  margin: 0;
  padding: 0.5em;
  text-align: center;
}
.channel-name {
  color: rgb(218, 216, 222);
}
"""

view model = 
  div []
    [ node "style" [] [ text css ]
    , case model.thanks of
        ThanksClip name clip ->
          div []
            [ displayName name
            , if model.showClip then
                displayClip clip
              else
                text clip.embedUrl
            ]
        Thanks name ->
          div []
            [ displayName name
            ]
        NoHosts -> text ""
    ]

displayName : String -> Html msg
displayName name =
  h1 []
    [ text "Thanks "
    , span [ class "channel-name" ] [ text name ]
    , text " for the host"
    ]

displayClip : Clip -> Html msg
displayClip clip =
  iframe
    [ src clip.embedUrl
    , sandbox "allow-scripts allow-same-origin"
    , attribute "allow" "autoplay"
    , attribute "width" "100%"
    , attribute "height" "500"
    , attribute "scrolling" "no"
    , attribute "frameborder" "0"
    ] []
