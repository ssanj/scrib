module ApiKeyPayloadDecoderSpec exposing (..)

import Test exposing (..)

import Expect      exposing (Expectation)
import Fuzz        exposing (Fuzzer, int, list, string)

import Json.Encode as E
import Json.Decode as D

import StorageKeys exposing (topNotesKey, keyValue, apiKeyKey)
import ApiKey exposing (..)

apiKeyDecoderWithApiKeyAndPayload : Test
apiKeyDecoderWithApiKeyAndPayload =
  test "can decode apiKey with payload" <|
    \_ ->
      let akPayloadDecoder = decodeApiKeyWithPayload topNotesKey D.string
          jsonData         =
            E.object
              [
                (keyValue apiKeyKey,   E.string "someApiKey")
              , (keyValue topNotesKey, E.string "Some payload")
              ]
          expected         = { apiKey =  ApiKey "someApiKey", payload = Just "Some payload" }
          result           = D.decodeValue akPayloadDecoder jsonData
       in case result of
            (Ok actual ) -> Expect.equal actual expected
            Err err      -> Expect.fail "Expected to successfully decode apiKey and payload"

apiKeyDecoderWithFailingPayload : Test
apiKeyDecoderWithFailingPayload =
  test "can decode an apiKey even if the payload fails to decode" <|
    \_ ->
      let akPayloadDecoder = decodeApiKeyWithPayload topNotesKey D.string
          jsonData         =
            E.object
              [
                (keyValue apiKeyKey,   E.string "another api key")
              , (keyValue topNotesKey, E.int 1)
              ]
          expected         = { apiKey = ApiKey "another api key", payload = Nothing }
          result           = D.decodeValue akPayloadDecoder jsonData
       in case result of
            (Ok actual ) -> Expect.equal actual expected
            Err err -> Expect.fail "Expected to successfully decode apiKey even if payload can't be decoded"

apiKeyDecoderWithoutApiKey : Test
apiKeyDecoderWithoutApiKey =
  test "fails to decode an ApiKeyPayload if the apiKey is missing" <|
    \_ ->
      let akPayloadDecoder = decodeApiKeyWithPayload topNotesKey D.string
          jsonData         =
            E.object
              [
               (keyValue topNotesKey, E.string "Some payload")
              ]
          result           = D.decodeValue akPayloadDecoder jsonData
      in case result of
           (Ok _)  -> Expect.fail "Expected to fail decoding apiKey"
           Err err -> Expect.true "Expected an error about a missing apiKey field" <| String.contains "Expecting an OBJECT with a field named `apiKey`" (D.errorToString err)

createSampleApiKeyWithPayload : String -> a -> ApiKeyWithPayload a
createSampleApiKeyWithPayload apiKey payload = ApiKeyWithPayload (ApiKey apiKey) (Just payload)
    --describe "The String module"
    --    [ describe "String.reverse" -- Nest as many descriptions as you like.
    --        [ test "has no effect on a palindrome" <|
    --            \_ ->
    --                let
    --                    palindrome =
    --                        "hannah"
    --                in
    --                    Expect.equal palindrome (String.reverse palindrome)

            ---- Expect.equal is designed to be used in pipeline style, like this.
            --, test "reverses a known string" <|
            --    \_ ->
            --        "ABCDEFG"
            --            |> String.reverse
            --            |> Expect.equal "GFEDCBA"

            ---- fuzz runs the test 100 times with randomly-generated inputs!
            --, fuzz string "restores the original string if you run it again" <|
            --    \randomlyGeneratedString ->
            --        randomlyGeneratedString
            --            |> String.reverse
            --            |> String.reverse
            --            |> Expect.equal randomlyGeneratedString
        --    ]
        --]
