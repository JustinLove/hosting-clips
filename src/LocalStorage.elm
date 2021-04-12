port module LocalStorage exposing (LoadResult(..), save, loaded, saveJson, loadedJson)

import Json.Encode
import Json.Decode

save : String -> Cmd msg
save = localStorageSave

saveJson : Json.Encode.Value -> Cmd msg
saveJson = Json.Encode.encode 0 >> localStorageSave

loaded : (Maybe String -> msg) -> Sub msg
loaded = localStorageLoaded

loadedJson : Json.Decode.Decoder a -> (LoadResult a -> msg) -> Sub msg
loadedJson decoder tagger =
  localStorageLoaded ((decodeLoaded decoder) >> tagger)

type LoadResult a
  = NoData
  | Data a
  | Error Json.Decode.Error

decodeLoaded : Json.Decode.Decoder a -> Maybe String -> LoadResult a
decodeLoaded decoder mstring =
  case mstring of
    Just string ->
      case Json.Decode.decodeString decoder string of
        Ok data -> Data data
        Err err -> Error err
    Nothing ->
      NoData

port localStorageSave : String -> Cmd msg
port localStorageLoaded : (Maybe String -> msg) -> Sub msg
