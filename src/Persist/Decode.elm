module Persist.Decode exposing (persist, clip)

import Persist exposing (Persist, Clip)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Time exposing (Time)

persist : Decoder Persist
persist =
  map3 Persist
    (field "exclusions" (list string))
    (field "durations" (keyValuePairs float))
    ( oneOf
      [ (field "clipCache" clipCache)
      , succeed Dict.empty
      ]
    )

clipCache : Decoder (Dict String (Time, List Clip))
clipCache =
  dict
    <| map2 (,)
      (field "time" float)
      (field "clips" (list clip))

clip : Decoder Clip
clip =
  map5 Clip
    (field "id" string)
    (field "url" string)
    (field "embedUrl" string)
    (field "broadcasterId" string)
    (succeed Nothing)

