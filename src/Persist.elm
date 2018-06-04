module Persist exposing (Persist, Clip)

import Dict exposing (Dict)
import Time exposing (Time)

type alias Persist =
  { exclusions : List String
  , durations : List (String, Time)
  , clipCache : Dict String (Time, List Clip)
  }

type alias Clip =
  { id : String
  , url : String
  , embedUrl : String
  , broadcasterId : String
  , duration : Maybe Float
  }

