module Persist.Encode exposing (persist, clip)

import Persist exposing (Persist, Clip)

import Json.Encode exposing (..)
import Dict exposing (Dict)
import Time exposing (Posix)

persist : Persist -> Value
persist p =
  object
    [ ("exclusions", list string p.exclusions)
    , ("durations", object <| List.map (\(id, time) -> (id, int time)) p.durations)
    , ("clipCache", clipCache p.clipCache)
    ]

clipCache : Dict String (Posix, List Clip) -> Value
clipCache =
  Dict.toList
    >> List.map (\(id, (time, clips)) ->
        (id, object
          [ ("time", int <| Time.posixToMillis time)
          , ("clips", list clip clips)
          ]
        )
      )
    >> object

clip : Clip -> Value
clip c =
  [ ("id", string c.id)
  , ("url", string c.url)
  , ("embedUrl", string c.embedUrl)
  , ("broadcasterId", string c.broadcasterId)
  ]
    |> List.append (case c.videoUrl of
      Just videoUrl -> [("videoUrl", string videoUrl)]
      Nothing -> []
    )
    |> object
