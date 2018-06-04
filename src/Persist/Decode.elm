module Persist.Decode exposing (persist)

import Persist exposing (Persist)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map2 Persist
    (field "exclusions" (list string))
    ( oneOf
      [ (field "durations" (keyValuePairs float))
      , succeed []
      ]
    )
