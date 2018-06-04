module Persist.Encode exposing (persist)

import Persist exposing (Persist)

import Json.Encode exposing (..)

persist : Persist -> Value
persist p =
  object
    [ ("exclusions", list <| List.map string p.exclusions)
    , ("durations", object <| List.map (\(id, time) -> (id, float time)) p.durations)
    ]
