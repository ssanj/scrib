port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model = String


init : () -> (Model, Cmd Msg)
init _ = ("", Cmd.none)


-- UPDATE


type Msg = NoteSaved
         | NoteEdited String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoteSaved             -> (model, sendSaveMessage model)
    (NoteEdited noteText) -> (noteText, sendMessage noteText)


-- VIEW


view : Model -> Html Msg
view model =

  section [class "section"]
    [
      div [class "container"]
        [
          h1 [class "title"] [text "Scrib"]
        , p [class "subtitle"] [text "Making scribbling effortless"]
        , div []
            [
              button [id "view-notes-button", class "button", class "is-text"]
                [text "View Notes"]
            , textarea
                [id "note-content", class "textarea", rows 10, placeholder "e.g. My awesome idea", onInput NoteEdited]
                [text model]
            , div [class "field", class "is-grouped"]
                [
                  p [class "control"]
                    [
                      button [id "save-note", onClick NoteSaved, class "button", class "is-success"]
                        [text "Save"]
                    ]
                  ]
            ]

        , viewMarkdownPreview
        ]
    ]


viewMarkdownPreview : Html msg
viewMarkdownPreview =
  div []
    [
      hr [] []
    , div [id "markdown-view"] []
    ]

-- PORTS

port sendMessage : String -> Cmd msg
port sendSaveMessage : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none