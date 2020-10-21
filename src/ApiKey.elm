module ApiKey exposing (..)

import Http
import Json.Decode as D
import StorageKeys exposing (JsonKey, keyValue, apiKeyKey)

-- TODO: should this be opaque? We would need to update the http
type alias ApiKey = { value: String }

apiKeyHeader: ApiKey -> Http.Header
apiKeyHeader apiKey = Http.header "X-API-KEY" apiKey.value

decodeApiKey : D.Decoder ApiKey
decodeApiKey = D.map ApiKey D.string

performApiKey : Maybe ApiKey -> (m, (ApiKey -> Cmd a)) -> (m, Cmd a) -> (m, Cmd a)
performApiKey maybeApiKey (model1, apiKeyCmd) (model2, nonApiKeyCmd) =
  case maybeApiKey of
    Just apiKey -> (model1, apiKeyCmd apiKey)
    Nothing     -> (model2, nonApiKeyCmd)

-- TODO: Does this belong here?
type alias ApiKeyWithPayload a =
  {
    apiKey: ApiKey
  , payload: Maybe a -- allows us to load the apiKey even if the payload fails to load
  }

decodeApiKeyWithPayload: JsonKey -> D.Decoder a -> D.Decoder (ApiKeyWithPayload a)
decodeApiKeyWithPayload key payloadDecoder =
  D.map2
    ApiKeyWithPayload
    (D.field (keyValue apiKeyKey) decodeApiKey)
    -- the maybe has to be on the outside to allow for:
    -- a missing field
    -- the encoding to fail
    (D.maybe (D.field (keyValue key) payloadDecoder))
