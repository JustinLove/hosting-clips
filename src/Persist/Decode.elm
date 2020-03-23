module Persist.Decode exposing (persist, clip)

import Persist exposing (Persist, Clip)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Time exposing (Posix)

persist : Decoder Persist
persist =
  map3 Persist
    (field "exclusions" (list string))
    (field "durations" (keyValuePairs int))
    ( oneOf
      [ (field "clipCache" clipCache)
      , succeed Dict.empty
      ]
    )

clipCache : Decoder (Dict String (Posix, List Clip))
clipCache =
  dict
    <| map2 Tuple.pair
      (field "time" (map Time.millisToPosix int))
      (field "clips" (list clip))

clip : Decoder Clip
clip =
  map6 Clip
    (field "id" string)
    (field "url" string)
    (field "embedUrl" string)
    (field "broadcasterId" string)
    (succeed Nothing)
    (maybe (field "videoUrl" string))

