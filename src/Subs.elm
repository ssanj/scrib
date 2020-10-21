module Subs exposing
  (
    handleJSResponse
  , SubType(..)
  , JsResponseEvent(..)
  , JsResponseError(..)
  )

import Json.Encode as E
import Json.Decode as D

type SubType = ViewSub JsResponseEvent

type JsResponseEvent = NoteSavedToLocalStorage
                     | NoteRemovedFromLocalStorage

type JsResponseError = JsResponseError String

handleJSResponse: (SubType -> msg) -> (String -> msg) -> E.Value -> msg
handleJSResponse successCallback errorCallback jsValue =
  let decoded: E.Value -> Result D.Error SubType
      decoded = D.decodeValue decoderJsResponseEvent

      handleDecoded: Result D.Error SubType -> msg
      handleDecoded result =
        case result of
          Ok event -> successCallback event
          Err err  -> errorCallback <| D.errorToString err
  in handleDecoded <| decoded jsValue

decoderJsResponseEvent: D.Decoder SubType
decoderJsResponseEvent = D.andThen stringToJsResponseEvent decodeJsResponseString

decodeJsResponseString :D.Decoder String
decodeJsResponseString = D.field "eventType" D.string

stringToJsResponseEvent: String -> D.Decoder SubType
stringToJsResponseEvent value =
  case value of
    "note_removed_from_local_storage" -> D.succeed <| ViewSub NoteRemovedFromLocalStorage -- we may need a better way to differentiate messages from js
    "note_saved_to_local_storage"     -> D.succeed <| ViewSub NoteSavedToLocalStorage
    other                             -> D.fail <| "Unknown JS Response type: " ++ other
