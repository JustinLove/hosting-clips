import Persist exposing (Persist)
import Persist.Decode
import Persist.Encode

import Expectation exposing (eql, isTrue, isFalse)
import Test exposing (it, describe, Test)
import Runner exposing (runAll)

import Html exposing (Html)
import Json.Decode

main : Html msg
main =
  runAll all

all : Test
all = describe "serialization"
  [ it "roundtrips persist" <| roundTrips
    Persist.Encode.persist
    Persist.Decode.persist
    ( Persist
      [ "clip id" ]
      [ ("clip id", 1) ]
    )
  ]

roundTrips : (a -> Json.Decode.Value) -> Json.Decode.Decoder a -> a -> Expectation.Expectation
roundTrips encoder decoder value =
  eql
    (Ok value)
    (value |> encoder |> Json.Decode.decodeValue decoder)
