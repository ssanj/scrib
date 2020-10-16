module ApiKey exposing (..)

import Http
import Json.Decode as D

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
