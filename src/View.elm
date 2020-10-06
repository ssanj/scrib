port module View exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import FP exposing (maybe, const)
import ElmCommon exposing (onlyModel, plainDiv)

import Json.Decode as D
import Json.Encode as E

import Debug exposing(toString, log)

-- MAIN

main: Program () Model Msg
main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }


-- MODEL

type alias Note =
  {
    noteText: String
  , noteId: Int
  }


type alias Model =
  {
    query: Maybe String
  , notes: List Note
  , selectedNote: Maybe Note
  }


type PortType = PreviewMessage

emptyModel: Model
emptyModel = Model Nothing []  Nothing


init : () -> (Model, Cmd Msg)
init _ = onlyModel emptyModel


-- UPDATE


type Msg = NoteSelected Note
         | NoteEdited Note
         | SearchEdited
         | SearchPerformed

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    (NoteSelected note) ->
      let x = log "NoteSelected" "-"
      in ({model| selectedNote = Just note }, scribMessage (encode PreviewMessage note))
    (NoteEdited note)   ->
      let x = log "NoteEdited: " (toString note)
      in onlyModel model
    _ ->
      let x = log "Other!" "moo"
      in onlyModel model


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ section [ class "section" ]
      [ div [ class "container" ]
        [ h1 [ class "title" ]
          [ text "Scrib" ]
        , p [ class "subtitle" ]
          [ text "Making scribbling effortless" ]
        , div []
          [ article [ class "panel", class "is-primary" ]
            [ p [ class "panel-heading" ]
              [ text "Saved Notes" ]
            , p [ class "panel-tabs" ]
              [ button [ class "button", class "is-text", id "add-note-button" ]
                [ text "Add Note" ]
              ]
            , div [ class "panel-block" ]
              [ p [ class "control has-icons-left" ]
                [ input [ class "input", class "is-primary", placeholder "Search", type_ "text" ]
                  []
                , span [ class "icon is-left" ]
                  [ i [ attribute "aria-hidden" "true", class "fas", class "fa-search" ]
                    []
                  ]
                ]
              ]
            , div [ id "notes-list" ]
                (List.map createNoteItem [1,2,3,4,5,6,7,8,9,10])
            ]
          ]
        ]
      ]
   , createEditButton (model.selectedNote)
   ]


createEditButton: Maybe Note -> Html Msg
createEditButton = maybe (viewMarkdownPreviewDefault) viewMarkdownPreview

createNoteItem: Int -> Html Msg
createNoteItem model =
  a [class "panel-block", onClick (NoteSelected ({ noteText = ("# some note-" ++ String.fromInt(model)), noteId = model }))]
  [ span [class "panel-icon"]
    [ i [ class "fas", class "fa-book", attribute "aria-hidden" "true"]
      []
    ]
  , text ("testing " ++ (String.fromInt(model)))
   ]

viewMarkdownPreview : Note -> Html Msg
viewMarkdownPreview note =
  div []
    [ hr []
      []
    , div [ id "preview" ]
      [ div [ id "markdown-view" ]
        []
      , button [ class "button", class "is-info", onClick (NoteEdited note) ]
        [ text "Edit" ]
      ]
    ]

viewMarkdownPreviewDefault: Html Msg
viewMarkdownPreviewDefault =
  div [] [ hr [] [] ]

-- PORTS

port scribMessage : E.Value -> Cmd msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- JSON ENCODE/DECODE

showPortType: PortType -> String
showPortType portType =
  case portType of
    PreviewMessage -> "preview_message"


encode : PortType -> Note -> E.Value
encode portType model =
  E.object
    [ ("eventType", E.string (showPortType portType))
    , ("noteText", E.string model.noteText)
    , ("noteId", E.int model.noteId)
    ]

---- field : String -> Decoder a -> Decoder a
---- maybe : Decoder a -> Decoder (Maybe a)

--decoder : D.Decoder Model
--decoder =
--  D.map2 Model
--    (D.field "noteText" D.string)
--    (D.maybe (D.field "noteId" D.int))