module Persist.Decode exposing (persist, clip)

import Persist exposing (Persist, Clip, UserId)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Time exposing (Posix)

persist : Decoder Persist
persist =
  map4 Persist
    (field "exclusions" (list string))
    (field "durations" (keyValuePairs int))
    ( oneOf
      [ (field "clipCache" clipCache)
      , succeed Dict.empty
      ]
    )
    ( oneOf
      [ (field "nameCache" nameCache)
      , succeed Dict.empty
      ]
    )

clipCache : Decoder (Dict UserId (Posix, List Clip))
clipCache =
  dict
    <| map2 Tuple.pair
      (field "time" (map Time.millisToPosix int))
      (field "clips" (list clip))

clip : Decoder Clip
clip =
  succeed Clip
    |> map2 (|>) (field "id" string)
    |> map2 (|>) (field "url" string)
    |> map2 (|>) (field "embedUrl" string)
    |> map2 (|>) (field "broadcasterId" string)
    |> map2 (|>) (maybe (field "broadcasterName" string))
    |> map2 (|>) (maybe (field "duration" int))
    |> map2 (|>) (maybe (field "videoUrl" string))

nameCache : Decoder (Dict UserId (Posix, String))
nameCache =
  dict
    <| map2 Tuple.pair
      (field "time" (map Time.millisToPosix int))
      (field "name" string)
