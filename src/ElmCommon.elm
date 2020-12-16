module ElmCommon exposing (..)

import Html            exposing (Html, div, text, Attribute, article, p, strong, br, button)
import Html.Attributes exposing (class, classList, id, attribute)
import Html.Events     exposing (onClick)
import Json.Encode     exposing (Value)

import List.Nonempty as N
import Json.Decode   as D
import Http          as Http

import FP exposing (maybe, maybeToBool)

type alias ErrorMessage = { errorMessage : String }

type alias InformationMessage = { infoMessage : String }

type alias SuccessMessage = { successMessage : String }

type alias Seconds = { seconds : Int }

type alias Encoder a = a -> Value

type alias ModelCommand model msg = model -> (model, Cmd msg)

onlyModel : a -> (a, Cmd msg)
onlyModel model = (model, Cmd.none)


-- HTML


plainDiv : List (Html msg) -> Html msg
plainDiv = div []

emptyDiv : Html a
emptyDiv = div [] []

addClasses : List String -> List (Attribute a)
addClasses classes = List.map class classes

hideAlertSpace : Html a
hideAlertSpace =
  div
    (
      addClasses [
        "px-1"
      , "py-1"
      , "has-text-white"
      , "has-background-white"
      , "is-invisible"
      ]
    )
    [
      text "placeholder"
    ]

--TODO: Combines these into one function that is called separately
addInlineInfoFlash : InformationMessage -> Html a
addInlineInfoFlash { infoMessage } =
  div
    (
      addClasses
      [
        "px-1"
      , "py-1"
      , "has-background-info"
      , "has-text-white"
      ]
    )
    [
      text infoMessage
    ]

addInlineSuccessFlash : SuccessMessage -> Html a
addInlineSuccessFlash {  successMessage } =
  div
    (
      addClasses
      [
        "px-1"
      , "py-1"
      , "has-background-primary"
      , "has-text-white"
      ]
    )
    [
      text successMessage
    ]

addInlineErrorFlash : ErrorMessage -> Html a
addInlineErrorFlash { errorMessage } =
  div
    (
      addClasses [
        "px-1"
      , "py-1"
      , "has-background-danger"
      , "has-text-white"
      ]
    )
    [
      text errorMessage
    ]

openErrorModal : N.Nonempty ErrorMessage -> a -> Html a
openErrorModal errorMessages event =
  div [ classList [ ("modal", True), ("is-active", True) ], id "error-modal" ]
    [ div [ class "modal-background" ]
        []
    , div [ class "modal-content" ]
        [ div [ class "box" ]
            [ article [ class "media" ]
                [ div [ class "media-content" ]
                    [ div [ class "content" ]
                          (N.toList <| N.map createErrorBlock errorMessages)
                    ]
                ]
            ]
        ]
    , button [ id "error-modal-close", attribute "aria-label" "close", class "modal-close is-large", onClick event ]
        []
    ]

createErrorBlock : ErrorMessage -> Html a
createErrorBlock { errorMessage } =
  p []
    [
      text errorMessage
    ]


foldResult : (D.Error -> b) -> (a -> b) -> Result D.Error a ->  b
foldResult failure success result =
  case result of
    (Ok value)  -> success value
    (Err error) -> failure error


-- HTTP


fromHttpError: Http.Error -> String
fromHttpError error =
  case error of
    (Http.BadUrl burl)      -> "bad url: " ++ burl
    Http.Timeout            -> "timeout"
    Http.NetworkError       -> "network error"
    (Http.BadStatus status) -> "bad status: " ++ String.fromInt status
    (Http.BadBody body)     -> "bad body: " ++ body