module Persist exposing (Persist, Clip, DurationInMilliseconds, UserId, ClipId)

import Dict exposing (Dict)
import Time exposing (Posix)

type alias Persist =
  { exclusions : List ClipId
  , durations : List (ClipId, DurationInMilliseconds)
  , clipCache : Dict UserId (Posix, List Clip)
  , nameCache : Dict UserId (Posix, String)
  }

type alias Clip =
  { id : ClipId
  , url : String
  , embedUrl : String
  , broadcasterId : UserId
  , broadcasterName : Maybe String
  , duration : Maybe DurationInMilliseconds
  , videoUrl : Maybe String
  }

type alias DurationInMilliseconds = Int
type alias UserId = String
type alias ClipId = String

