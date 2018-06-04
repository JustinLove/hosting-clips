module Persist exposing (Persist)

import Dict exposing (Dict)
import Time exposing (Time)

type alias Persist =
  { exclusions : List String
  , durations : List (String, Time)
  }
