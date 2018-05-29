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
.view {
  height: 100%;
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
"""

view model = 
  div [ class "view" ]
    [ node "style" [] [ text css ]
    , case model.thanks of
        ThanksClip name clip ->
          div [ class "host clip" ]
            [ displayName name
            , if model.showClip then
                displayClip clip
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
    , attribute "height" "600"
    , attribute "scrolling" "no"
    , attribute "frameborder" "0"
    ] []
