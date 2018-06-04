port module Harbor exposing (..)

port save : String -> Cmd msg
port loaded : (Maybe String -> msg) -> Sub msg
