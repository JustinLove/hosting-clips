module View exposing (Msg(..), Clip, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Time exposing (Time)

type Msg
  = None

type alias Clip =
  { id : String
  , embedUrl : String
  }

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
"""

view model = 
  div []
    [ case model.displayedBroadcaster of
        Just name -> text ("Thanks " ++ name ++ " for the host")
        Nothing -> text ""
    , case model.displayedClip of
      Just clip ->
        iframe
          [ src clip.embedUrl
          , sandbox "allow-scripts allow-same-origin"
          , attribute "allow" "autoplay"
          , attribute "width" "100%"
          , attribute "height" "500"
          , attribute "scrolling" "no"
          , attribute "frameborder" "0"
          ] []
      Nothing -> text ""
    ]
