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
      --   "data": A -- could be any Json value including `null`
      --   "error": "some error" -- optional
      -- }
-}

errorKeyField : String
errorKeyField = "error"

type ResponseJson = SuccessResponse String E.Value
                  | ErrorResponse String String

encodeJsResponse : (JsResponse E.Value -> msg) -> (String -> msg) -> E.Value -> msg
encodeJsResponse successCallback errorCallback jsonValue =
    let jsResponseWithValueDecoder = (D.andThen decodeJsResponse decodeResponseJson) -- D.Decoder (JsResponse E.Value)
        decodeResult = D.decodeValue jsResponseWithValueDecoder jsonValue            -- Result Error (JsResponse E.Value)
    in case decodeResult of
        Ok (JsResponse responseKey payload) -> successCallback <| JsResponse responseKey payload
        Err err                             -> errorCallback <| D.errorToString err

decodeJsResponse : ResponseJson -> D.Decoder (JsResponse E.Value)
decodeJsResponse responseJson =
  case responseJson of
    (SuccessResponse responseKey payload) ->
      let responseKeyDecoder = D.map P.ResponseKey <| D.field P.responseKeyField D.string
      in D.map ((flip JsResponse) payload) responseKeyDecoder

    (ErrorResponse responseKey error)     -> D.fail error


decodeResponseJson: D.Decoder ResponseJson
decodeResponseJson = D.oneOf [decodeErrorResponse, decodeSuccessResponse]

decodeSuccessResponse :D.Decoder ResponseJson
decodeSuccessResponse =
  let responseType = D.field P.responseKeyField D.string
      payload      = D.field P.dataKeyField D.value
  in D.map2
      SuccessResponse
      responseType
      payload

decodeErrorResponse :D.Decoder ResponseJson
decodeErrorResponse =
  let responseType = D.field P.responseKeyField D.string
      error        = D.field errorKeyField D.string
  in D.map2
      ErrorResponse
      responseType
      error

foldResult : (a -> c) -> (b -> c) -> Result a b ->  c
foldResult onError onSuccess result =
  case result of
    Ok value -> onSuccess value
    Err err  -> onError err