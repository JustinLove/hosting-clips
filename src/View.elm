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
    [ text "view"
    , text <| toString model.clips
    ]
