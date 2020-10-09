port module Save exposing (..)

import Browser
import Http
import Html exposing (..)
import RemoteData exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import FP exposing (maybe, const)
import ElmCommon exposing (onlyModel, plainDiv)

import Json.Decode as D
import Json.Encode as E
import Note        as N
import Browser.Navigation


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


type alias RemoteSaveData = WebData Int

type alias Model =
  {
    noteText: String
  , noteId: RemoteSaveData
  }


init : E.Value -> (Model, Cmd Msg)
init json =
  let result = D.decodeValue decoder json
  in case result of
    Ok model  -> onlyModel model
    Err _     -> onlyModel defaultModel

defaultModel: Model
defaultModel = Model "" NotAsked

-- UPDATE

type PortType = SaveMessage
              | PreviewMessage


type Msg = NoteSaved
         | NoteEdited String
         | NewNote
         | ViewNote
         | NoteSaveResponse RemoteSaveData


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoteSaved             -> saveNote model
      --let newModel = { model | noteId = Just 10 }
      --in (newModel, scribMessage (encode SaveMessage newModel))
    (NoteEdited noteText) ->
       let updatedModel = { model | noteText = noteText }
       in (updatedModel, scribMessage (encode PreviewMessage updatedModel))
    NewNote               ->
      let updatedModel = { model | noteText = "", noteId = NotAsked }
      in (updatedModel, scribMessage (encode PreviewMessage updatedModel))

    ViewNote              -> (model, Browser.Navigation.load "view.html")
    (NoteSaveResponse noteResponse) -> onlyModel {model | noteId = noteResponse}

saveNote: Model -> (Model, Cmd Msg)
saveNote model =
  case model.noteId of
    Loading       -> (model, Cmd.none) -- still loading from a previous save...
    _             -> performSaveNote model


performSaveNote: Model -> (Model, Cmd Msg)
performSaveNote model =
    let remoteCall =
          Http.post {
            url = "http://localhost:3000/note"
          , body = Http.jsonBody <| encodeSaveNote model
          , expect = Http.expectJson (RemoteData.fromResult >> NoteSaveResponse) D.int
          }
    in ({model | noteId = Loading }, remoteCall)


encodeSaveNote: Model -> E.Value
encodeSaveNote {noteText, noteId} =
  let maybeId = remoteDataToMaybe noteId
  in maybe (encodeUnsavedNote noteText) (encodeSavedNote noteText) maybeId


encodeSavedNote: String -> Int -> E.Value
encodeSavedNote noteText noteId = N.encodeNote <| N.Note noteText noteId

encodeUnsavedNote: String -> E.Value
encodeUnsavedNote noteText =
  E.object
    [
      ("noteText", E.string noteText)
    ]

remoteDataToMaybe: RemoteData e a -> Maybe a
remoteDataToMaybe remoteData =
  case remoteData of
    Success a -> Just a
    _ -> Nothing

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


fromHttpError: Http.Error -> String
fromHttpError error =
  case error of
    (Http.BadUrl burl)      -> "bad url: " ++ burl
    Http.Timeout            -> "timeout"
    Http.NetworkError       -> "network error"
    (Http.BadStatus status) -> "bad status: " ++ String.fromInt status
    (Http.BadBody body)     -> "bad body: " ++ body

viewNoteEditingArea : Model -> Html Msg
viewNoteEditingArea model =
  plainDiv
    [
      viewNotificationsArea model
    , viewNotesTextArea model
    , viewControls model
    ]


viewNotificationsArea: Model -> Html a
viewNotificationsArea {noteId} =
  case noteId of
    Failure e -> div [] [text <| "Save failed: " ++ fromHttpError e]
    Success _ -> div [] [text "Saved note"]
    _         -> div [style "visibility" "hidden"] [text "."]

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
          viewSaveButton model
        , button [
            id "new-note"
          , onClick NewNote
          , classList
              [("button", True), ("is-text", True){--, ("is-hidden", not (hasBeenSaved model))--}]
          ]
            [text "New Note"]
        ]
        , button [id "view-notes-button", class "button", class "is-text", onClick ViewNote]
            [text "View Notes"]

    ]


viewSaveButton: Model -> Html Msg
viewSaveButton model =
  let showSpinner =
        case model.noteId of
          Loading -> True
          _       -> False
  in button [
       id "save-note"
       , onClick NoteSaved
       , classList
           [
             ("button", True)
           , ("is-success", True)
           , ("is-static", not (hasContent model))
           , ("is-loading", showSpinner)
           ]
     ]
      [text (saveButtonText model)]



hasContent: Model -> Bool
hasContent {noteText} = not (String.isEmpty noteText)

hasBeenSaved: Model -> Bool
hasBeenSaved {noteId} = maybe False (const True) (remoteDataToMaybe noteId)

saveButtonText : Model -> String
saveButtonText {noteId} = maybe "Save" (const "Edit") (remoteDataToMaybe noteId)

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
    , ("noteId", maybe E.null E.int <| remoteDataToMaybe model.noteId)
    ]

-- field : String -> Decoder a -> Decoder a
-- maybe : Decoder a -> Decoder (Maybe a)

decoder : D.Decoder Model
decoder =
  D.map2 Model
    (D.field "noteText" D.string)
    (D.map maybeToRemoteData <| D.maybe (D.field "noteId" D.int))

maybeToRemoteData : Maybe a -> RemoteData e a
maybeToRemoteData maybeValue =
  maybe NotAsked Success maybeValue

