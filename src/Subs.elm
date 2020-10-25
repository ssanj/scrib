module Subs exposing
  (
    -- Data types
    JsResponse(..)

    -- Functions
  , encodeJsResponse
  )

import FP exposing (flip)

import Json.Encode as E
import Json.Decode as D
import Ports       as P

type JsResponse a = JsResponse P.ResponseKey a

{-
  Sample json sent from Javascript Subs:
      -- {
      --   "responseKey": "some key",
      --   "data": A -- could be any Json value.
      -- }
-}

type ResponseJson = ResponseJson String E.Value

encodeJsResponse : D.Decoder a -> (JsResponse a -> msg) -> (String -> msg) -> E.Value -> msg
encodeJsResponse payloadDecoder successCallback errorCallback jsonValue =
    let jsResponseWithValueDecoder = (D.andThen decodeJsResponse decodeResponseJson) -- D.Decoder (JsResponse E.Value)
        decodeResult = D.decodeValue jsResponseWithValueDecoder jsonValue            -- Result Error (JsResponse E.Value)
    in case decodeResult of
        Ok (JsResponse responseKey payload) ->
          let payloadDecodeResult = D.decodeValue payloadDecoder payload
          in foldResult (errorCallback << D.errorToString) (successCallback << JsResponse responseKey) payloadDecodeResult
        Err err      -> errorCallback <| D.errorToString err

decodeJsResponse : ResponseJson -> D.Decoder (JsResponse E.Value)
decodeJsResponse (ResponseJson responseKey payload) =
  let responseKeyDecoder = D.map P.ResponseKey <| D.field P.responseKeyField D.string
  in D.map ((flip JsResponse) payload) responseKeyDecoder

decodeResponseJson :D.Decoder ResponseJson
decodeResponseJson =
  let responseType = D.field P.responseKeyField D.string
      payload      = D.field P.dataKeyField D.value
  in D.map2
      ResponseJson
      responseType
      payload

foldResult : (a -> c) -> (b -> c) -> Result a b ->  c
foldResult onError onSuccess result =
  case result of
    Ok value -> onSuccess value
    Err err  -> onError err