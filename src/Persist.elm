module Persist exposing (Persist, Clip, DurationInMilliseconds)

import Dict exposing (Dict)
import Time exposing (Posix)

type alias Persist =
  { exclusions : List String
  , durations : List (String, DurationInMilliseconds)
  , clipCache : Dict String (Posix, List Clip)
  }

type alias Clip =
  { id : String
  , url : String
  , embedUrl : String
  , broadcasterId : String
  , duration : Maybe DurationInMilliseconds
  }

type alias DurationInMilliseconds = Int
