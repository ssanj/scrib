module ElmCommon exposing (..)

import Html            exposing (Html, div, text, Attribute, article, p, strong, br, button)
import Html.Attributes exposing (class, classList, id, attribute)
import Html.Events     exposing (onClick)
import Json.Encode     exposing (Value)
import Debug

import List.Nonempty as N
import FP exposing (maybe, maybeToBool)

type alias ErrorMessage = { errorMessage : String }

type alias InformationMessage = { infoMessage : String }

type alias SuccessMessage = { successMessage : String }


type alias Encoder a = a -> Value

debug : String -> ()
debug message = Debug.log message ()

debugWith : String -> a -> a
debugWith = Debug.log

onlyModel : a -> (a, Cmd msg)
onlyModel model = (model, Cmd.none)

plainDiv : List (Html msg) -> Html msg
plainDiv = div []

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

emptyDiv : Html a
emptyDiv = div [] []