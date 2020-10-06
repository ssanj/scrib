module ElmCommon exposing (..)

import Html exposing (Html, div)

onlyModel: a -> (a, Cmd msg)
onlyModel model = (model, Cmd.none)

plainDiv: List (Html msg) -> Html msg
plainDiv = div []