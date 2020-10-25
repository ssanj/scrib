module Subs exposing
  (
    encodeJsResponse
  --, SubType(..)
  --, JsResponseEvent(..)
  --, JsResponseError(..)
  )

import Json.Encode as E
import Json.Decode as D

fromStorage : String
fromStorage = "storage_response"

type EventSource = FromStorage
                 | Unknown

type JsResponse a = JsResponse EventSource a

type ResponseJson = ResponseJson String E.Value

encodeJsResponse : E.Value -> D.Decoder a -> (JsResponse a -> msg) -> (String -> msg) -> msg
encodeJsResponse jsonValue payloadDecoder successCallback errorCallback =
    let jsResponseWithValueDecoder = (D.andThen decodeJsResponse decodeResponseJson) -- D.Decoder (JsResponse E.Value)
        decodeResult = D.decodeValue jsResponseWithValueDecoder jsonValue            -- Result Error (JsResponse E.Value)
    in case decodeResult of
        Ok (JsResponse es payload) ->
          let payloadDecodeResult = D.decodeValue payloadDecoder payload
          in foldResult (errorCallback << D.errorToString) (successCallback << JsResponse es) payloadDecodeResult
        Err err      -> errorCallback <| D.errorToString err

decodeJsResponse : ResponseJson -> D.Decoder (JsResponse E.Value)
decodeJsResponse (ResponseJson eventSource payload) =
  let eventSourceDecoder = getEventSource eventSource
  in D.map (\es -> JsResponse es payload) eventSourceDecoder

decodeResponseJson :D.Decoder ResponseJson
decodeResponseJson =
  let responseType = D.field "responseType" D.string
      payload      = D.field "data" D.value
  in D.map2
      ResponseJson
      responseType
      payload

getEventSource : String -> D.Decoder EventSource
getEventSource eventSource =
  case eventSource of
    "storage_response" -> D.succeed FromStorage
    _ -> D.fail <| "Unknown event type: " ++ eventSource


foldResult : (a -> c) -> (b -> c) -> Result a b ->  c
foldResult onError onSuccess result =
  case result of
    Ok value -> onSuccess value
    Err err  -> onError err