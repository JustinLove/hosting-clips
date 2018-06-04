module Persist.Decode exposing (persist)

import Persist exposing (Persist)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map Persist
    (field "exclusions" (list string))
