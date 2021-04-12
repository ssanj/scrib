module Component.Footer exposing (view)

import Html            exposing (..)
import Html.Attributes exposing (..)

version = "3.0.8"

view : Html msg
view =
  nav
    [ attribute "aria-label" "main navigation", class "content", attribute "role" "navigation" ]
    [ div
        [ class "content has-text-centered" ]
        [ p
            [ class "scrib-footer"]
            [
              text "scribble effortlessly"
            ]
        , div
          [ class "is-size-7" ]
          [
            text "crafted by "
          , a
              [ href "https://sanj.ink"]
              [
                text "Sanj Sahayam"
              ]
          ]
        , div
          [ class "is-small-8" ]
          [
            text version
          ]
        ]
    ]