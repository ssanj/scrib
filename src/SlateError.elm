module SlateError exposing
  (
    -- DATA TYPES

    SlateError

    -- DECODERS

  , decodeSlateError

    -- FUNCTIONS

  , showSlateError
  )

import Json.Decode as D

type alias SlateError =
  {
    errorId : Int
  , errorMessage : String
  }

decodeSlateError : D.Decoder SlateError
decodeSlateError =
  D.map2
    SlateError
    (D.field "errorId" D.int)
    (D.field "errorMessage" D.string)

showSlateError : SlateError -> String
showSlateError { errorId , errorMessage } = "errorId: " ++ (String.fromInt errorId) ++ ", errorMessage: " ++ errorMessage