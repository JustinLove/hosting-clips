module Persist.Encode exposing (persist, clip)

import Persist exposing (Persist, Clip)

import Json.Encode exposing (..)
import Dict exposing (Dict)
import Time exposing (Time)

persist : Persist -> Value
persist p =
  object
    [ ("exclusions", list <| List.map string p.exclusions)
    , ("durations", object <| List.map (\(id, time) -> (id, float time)) p.durations)
    , ("clipCache", clipCache p.clipCache)
    ]

clipCache : Dict String (Time, List Clip) -> Value
clipCache =
  Dict.toList
    >> List.map (\(id, (time, clips)) ->
        (id, object
          [ ("time", float time)
          , ("clips", list <| List.map clip clips)
          ]
        )
      )
    >> object

clip : Clip -> Value
clip c =
  object
    [ ("id", string c.id)
    , ("url", string c.url)
    , ("embedUrl", string c.embedUrl)
    , ("broadcasterId", string c.broadcasterId)
    ]
