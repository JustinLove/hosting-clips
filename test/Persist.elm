import Persist exposing (Persist, Clip)
import Persist.Decode
import Persist.Encode

import Expectation exposing (eql, isTrue, isFalse)
import Test exposing (it, describe, Test)
import Runner exposing (runAll)

import Html exposing (Html)
import Json.Decode
import Dict
import Time

main : Html msg
main =
  runAll all

all : Test
all = describe "serialization"
  [ it "roundtrips clip" <| roundTrips
    Persist.Encode.clip
    Persist.Decode.clip
    { id = "clip id"
    , url = "url"
    , embedUrl = "embed url"
    , broadcasterId = "host id"
    , duration = Nothing
    }
  , it "roundtrips persist" <| roundTrips
    Persist.Encode.persist
    Persist.Decode.persist
    ( Persist
      [ "clip id" ]
      [ ("clip id", 1) ]
      ( Dict.fromList
        [ ("host id",
            (Time.millisToPosix 1,
              [
                { id = "clip id"
                , url = "url"
                , embedUrl = "embed url"
                , broadcasterId = "host id"
                , duration = Nothing
                }
              ]
            )
          )
        ]
      )
    )
  ]

roundTrips : (a -> Json.Decode.Value) -> Json.Decode.Decoder a -> a -> Expectation.Expectation
roundTrips encoder decoder value =
  eql
    (Ok value)
    (value |> encoder |> Json.Decode.decodeValue decoder)
