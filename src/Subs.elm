module Subs exposing
  (
    encodeJsResponse
  --, SubType(..)
  --, JsResponseEvent(..)
  --, JsResponseError(..)
  )

import Json.Encode as E
import Json.Decode as D

import FP exposing (flip)

type SubName = SubName String

fromStorageSub : SubName
fromStorageSub = SubName "storage_response"

type JsResponse = FromStorage SubName E.Value

type ResponseJson = ResponseJson String E.Value

encodeJsResponse : E.Value -> (JsResponse -> msg) -> (String -> msg) -> msg
encodeJsResponse jsonValue successCallback errorCallback =
  let responseJsonDecoder           = decodeResponseJson                  -- D.Decoder ResponseJson
      decodeJsResponseResultDecoder = D.map decodeJsResponse responseJsonDecoder    -- D.Decoder (Result String JsResponse)
      decodeResult                  = D.decodeValue decodeJsResponseResultDecoder jsonValue          -- Result Error (Result String JsResponse)
      decodeResultWithStringError   = Result.mapError D.errorToString decodeResult -- Result String (Result String JsResponse)
      result = Result.andThen identity decodeResultWithStringError
  in case result of
      Ok value -> successCallback value
      Err er   -> errorCallback er

decodeJsResponse : ResponseJson -> Result String JsResponse
decodeJsResponse (ResponseJson subName payload) =
  let subNameResult = getSubName subName
  in Result.map (decoderJsResponse payload) subNameResult

decoderJsResponse : E.Value -> SubName -> JsResponse
decoderJsResponse = flip FromStorage

decodeResponseJson :D.Decoder ResponseJson
decodeResponseJson =
  let responseType = D.field "responseType" D.string
      payload      = D.field "data" D.value
  in D.map2
      ResponseJson
      responseType
      payload

responseFromSubName : SubName -> E.Value -> JsResponse
responseFromSubName = FromStorage

getSubName : String -> Result String SubName
getSubName possibleSubName =
  case possibleSubName of
    "storage_response" -> Ok fromStorageSub
    _ -> Err <| "Unknown event type: " ++ possibleSubName
