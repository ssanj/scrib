port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import FP exposing (maybe, const)

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  {
    noteText: String
  , noteId: Maybe Int
  }


init : () -> (Model, Cmd Msg)
init _ = (Model ""  Nothing, Cmd.none)


-- UPDATE


type Msg = NoteSaved
         | NoteEdited String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoteSaved             -> ({ model | noteId = Just 10 }, sendSaveMessage model.noteText)
    (NoteEdited noteText) -> ({ model | noteText = noteText }, sendMarkdownPreviewMessage noteText)


-- VIEW


view : Model -> Html Msg
view model =
  section [class "section"]
    [
      div [class "container"]
        (
          viewHeadings ++
          [
            viewNoteEditingArea model
          , viewMarkdownPreview
          ]
        )
    ]

viewHeadings : List (Html msg)
viewHeadings =
  [
    h1 [class "title"] [text "Scrib"]
  , p [class "subtitle"] [text "Making scribbling effortless"]
  ]


viewNoteEditingArea : Model -> Html Msg
viewNoteEditingArea model =
  div []
    [
      button [id "view-notes-button", class "button", class "is-text"]
        [text "View Notes"]
    , textarea
        [id "note-content", class "textarea", rows 10, placeholder "e.g. My awesome idea", onInput NoteEdited]
        [text model.noteText]
    , div [class "field", class "is-grouped"]
        [
          p [class "control"]
            [
              button [id "save-note", onClick NoteSaved, class "button", class "is-success"]
                [text (saveButtonText model)]
            ]
        ]
    ]



saveButtonText : Model -> String
saveButtonText {noteId} = maybe "Save" (const "Edit") noteId

viewMarkdownPreview : Html Msg
viewMarkdownPreview =
  div []
    [
      hr [] []
    , div [id "markdown-view"] []
    ]

-- PORTS

-- sent when you want to preview the current note as markdown
port sendMarkdownPreviewMessage : String -> Cmd msg

-- sent when you save a note
port sendSaveMessage : String -> Cmd msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none