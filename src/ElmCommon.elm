module ElmCommon exposing (..)

import Html exposing (Html, div, text, Attribute)
import Html.Attributes exposing (class)

onlyModel: a -> (a, Cmd msg)
onlyModel model = (model, Cmd.none)

plainDiv: List (Html msg) -> Html msg
plainDiv = div []

addClasses: List String -> List (Attribute a)
addClasses classes = List.map class classes

hideAlertSpace: Html a
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

addSuccessAlert: String -> Html a
addSuccessAlert textValue =
  div
    (
      addClasses
      [
        "px-1"
      , "py-1"
      , "has-background-primary"
      , "has-text-white"
      , "autohide"
      ]
    )
    [
      text textValue
    ]

addFailureAlert: String -> Html a
addFailureAlert textValue =
  div
    (
      addClasses [
        "px-1"
      , "py-1"
      , "has-background-danger"
      , "has-text-white"
      , "autohide"
      ]
    )
    [
      text textValue
    ]