module Component.Footer exposing (view)

import Html            exposing (..)
import Html.Attributes exposing (..)

version : String
version = "0.3.0.10"

view : Html msg
view =
  nav
    [
      attribute "aria-label" "main navigation"
    , class "content"
    , attribute "role" "navigation"
    , style "margin-top" "2em"
    , style "border-top" "2px solid #fafafa"
    ]
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