port module Save exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import FP exposing (maybe, const)

import Json.Decode as D
import Json.Encode as E

-- MAIN

main: Program E.Value Model Msg
main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }


-- MODEL


type alias Model =
  {
    noteText: String
  , noteId: Maybe Int
  }


init : E.Value -> (Model, Cmd Msg)
init json =
  let result = D.decodeValue decoder json
  in case result of
    Ok model  -> onlyModel model
    Err _     -> onlyModel defaultModel

defaultModel: Model
defaultModel = Model "" Nothing


onlyModel: Model -> (Model, Cmd msg)
onlyModel model = (model, Cmd.none)

-- UPDATE

type PortType = SaveMessage
              | PreviewMessage


type Msg = NoteSaved
         | NoteEdited String
         | NewNote


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoteSaved             -> ({ model | noteId = Just 10 }, scribMessage (encode SaveMessage model))
    (NoteEdited noteText) ->
       let updatedModel = { model | noteText = noteText }
       in (updatedModel, scribMessage (encode PreviewMessage updatedModel))
    NewNote               ->
      let updatedModel = { model | noteText = "", noteId = Nothing }
      in (updatedModel, scribMessage (encode PreviewMessage updatedModel))


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
  plainDiv
    [
      viewNotesTextArea model
    , viewControls model
    ]

plainDiv: List (Html msg) -> Html msg
plainDiv = div []

viewNotesTextArea: Model -> Html Msg
viewNotesTextArea model =
  textarea
    [id "note-content", class "textarea", rows 10, placeholder "e.g. My awesome idea", onInput NoteEdited, value model.noteText]
    []


viewControls: Model -> Html Msg
viewControls model =
  div [class "field", class "is-grouped"]
    [
      p [class "control"]
        [
          button [
            id "save-note"
            , onClick NoteSaved
            , classList
                [("button", True), ("is-success", True), ("is-static", not (hasContent model))]
          ]
            [text (saveButtonText model)]
        , button [
            id "new-note"
          , onClick NewNote
          , classList
              [("button", True), ("is-text", True), ("is-hidden", not (hasBeenSaved model))]
          ]
            [text "New Note"]
        ]
        , button [id "view-notes-button", class "button", class "is-text"]
            [text "View Notes"]

    ]

hasContent: Model -> Bool
hasContent {noteText} = not (String.isEmpty noteText)

hasBeenSaved: Model -> Bool
hasBeenSaved {noteId} = maybe False (const True) noteId

saveButtonText : Model -> String
saveButtonText {noteId} = maybe "Save" (const "Edit") noteId

viewMarkdownPreview : Html Msg
viewMarkdownPreview =
  plainDiv
    [
      hr [] []
    , div [id "markdown-view"] []
    ]

-- PORTS

port scribMessage : E.Value -> Cmd msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- JSON ENCODE/DECODE

showPortType: PortType -> String
showPortType portType =
  case portType of
    SaveMessage -> "save_message"
    PreviewMessage -> "preview_message"


encode : PortType -> Model -> E.Value
encode portType model =
  E.object
    [ ("eventType", E.string (showPortType portType))
    , ("noteText", E.string model.noteText)
    , ("noteId", maybe E.null E.int model.noteId)
    ]

-- field : String -> Decoder a -> Decoder a
-- maybe : Decoder a -> Decoder (Maybe a)

decoder : D.Decoder Model
decoder =
  D.map2 Model
    (D.field "noteText" D.string)
    (D.maybe (D.field "noteId" D.int))