port module View exposing (..)

import Browser
import Http
import RemoteData exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import FP exposing (maybe)
import ElmCommon exposing (onlyModel)

import Json.Decode as D
import Json.Encode as E
import Note        as N

import Debug exposing(toString, log)
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

type alias Model =
  {
    query: Maybe String
  , notes: RemoteNotesData
  , selectedNote: Maybe N.Note
  }

type alias RemoteNotesData = WebData (List N.Note)

type PortType = PreviewMessage
              | SaveToLocalStorage
              | SaveToSessionStorage
              | RemoveFromLocalStorage
              | LogToConsole

type JsResponseEvent = SavedToLocalStorage
                     | RemovedFromLocalStorage

emptyModel: Model
emptyModel = Model Nothing NotAsked Nothing


init : E.Value -> (Model, Cmd Msg)
init notes =
    let decodeResult = D.decodeValue N.decodeNotes notes
    in case decodeResult of
         Ok validNotes -> onlyModel { emptyModel | notes = Success validNotes }
         Err err       -> (
                            { emptyModel | notes = Loading }
                          , Cmd.batch [getRemoteNotes, logMessage <| "Could not load view data: " ++ D.errorToString err]
                          )

-- UPDATE


getRemoteNotes: Cmd Msg
getRemoteNotes =
  Http.get {
    url = "http://localhost:3000/notes"
  , expect = Http.expectJson (RemoteData.fromResult >> NotesResponse) N.decodeNotes
  }


type Msg = NoteSelected N.Note
         | NoteEdited N.Note
         | NoteSavedToLocalStorage
         | NoteRemovedFromLocalStorage
         | JSNotificationError String
         | AddNote
         | SearchEdited
         | SearchPerformed
         | NotesResponse RemoteNotesData



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    (NoteSelected note) ->
      let _ = log "NoteSelected" "-"
      in ({model| selectedNote = Just note }, scribMessage (encode PreviewMessage note))
    (NoteEdited note)   ->
      let _ = log "NoteEdited: " (toString note)
      in (model, scribMessage (encode SaveToLocalStorage note))
    NoteSavedToLocalStorage     -> (model, Browser.Navigation.load "save.html")
    NoteRemovedFromLocalStorage -> (model, Browser.Navigation.load "save.html")
    (JSNotificationError error) -> (model, scribMessage(encodeLogToConsole error))
    AddNote                     -> (model, scribMessage encodeRemoveFromLocalStorage)
    (NotesResponse notes)       -> ({ model | notes = notes}, logResponseErrors notes)
    SearchPerformed             -> ({ model | notes = Loading }, getRemoteNotes)
    _ ->
      let _ = log "Other!" "moo"
      in onlyModel model


logMessage: String -> Cmd Msg
logMessage = scribMessage << encodeLogToConsole

logResponseErrors: RemoteNotesData -> Cmd Msg
logResponseErrors remoteData =
  case remoteData of
    Failure e     -> scribMessage <| encodeLogToConsole <| fromHttpError e
    Success notes -> scribMessage <| encodeViewNotes notes
    _             -> Cmd.none


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
              [ text "Saved Notes"
              , text " "
              , span [class "tab", class "is-medium"] [text <| getNoteCount model.notes]
              ]
            , p [ class "panel-tabs" ]
              [ button [ class "button", class "is-text", onClick AddNote]
                [ text "Add Note" ]
              , button [ class "button", class "is-text", onClick SearchPerformed ]
                [ text "Refresh" ]
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
            , viewNotesList model.notes

            ]
          ]
        ]
      ]
   , createEditButton model.selectedNote
   ]

getNoteCount: RemoteNotesData -> String
getNoteCount remoteNoteData =
  case remoteNoteData of
    Success notes -> String.fromInt <| List.length notes
    _             -> "-"

viewNotesList: RemoteNotesData -> Html Msg
viewNotesList remoteNotesData =
  let notesContent =
        case remoteNotesData of
          NotAsked      -> [div [] [text "No Data"]]
          Loading       -> [div [] [text "Loading..."]]
          Failure e     -> [div [] [text <| "oops! Could not get your data :(" ++ fromHttpError e]]
          Success notes -> List.map createNoteItem notes
  in div [ id "notes-list" ] notesContent

fromHttpError: Http.Error -> String
fromHttpError error =
  case error of
    (Http.BadUrl burl)      -> "bad url: " ++ burl
    Http.Timeout            -> "timeout"
    Http.NetworkError       -> "network error"
    (Http.BadStatus status) -> "bad status: " ++ String.fromInt status
    (Http.BadBody body)     -> "bad body: " ++ body


createEditButton: Maybe N.Note -> Html Msg
createEditButton = maybe viewMarkdownPreviewDefault viewMarkdownPreview

-- Update this to only create a note with the text for the first line
createNoteItem: N.Note -> Html Msg
createNoteItem {noteText, noteId} =
  a [class "panel-block", onClick (NoteSelected { noteText = noteText, noteId = noteId })]
  [ span [class "panel-icon"]
    [ i [ class "fas", class "fa-book", attribute "aria-hidden" "true"]
      []
    ]
  , text <| removeHeading <| onlyHeading noteText
   ]

onlyHeading : String -> String
onlyHeading note = maybe "-no title-" identity (List.head <| String.split "\n" note)

removeHeading : String -> String
removeHeading = String.replace "# " ""

viewMarkdownPreview : N.Note -> Html Msg
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
  div []
    [ hr []
      []
    , div [ id "preview" ]
      [ div [ id "markdown-view" ]
        []
      ]
    ]

-- PORTS

port scribMessage : E.Value -> Cmd msg
port jsMessage : (E.Value -> msg) -> Sub msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  let decoded: E.Value -> Result D.Error JsResponseEvent
      decoded = D.decodeValue decoderJsResponseEvent

      handleDecoded: Result D.Error JsResponseEvent -> Msg
      handleDecoded result =
        case result of
          Ok event ->
            case event of
               RemovedFromLocalStorage -> NoteRemovedFromLocalStorage
               SavedToLocalStorage     -> NoteSavedToLocalStorage
          Err err    -> JSNotificationError << D.errorToString <| err
  in jsMessage (handleDecoded << decoded)

-- JSON ENCODE/DECODE

showPortType: PortType -> String
showPortType portType =
  case portType of
    PreviewMessage         -> "preview_message"
    SaveToLocalStorage     -> "save_message"
    SaveToSessionStorage   -> "save_view_session"
    RemoveFromLocalStorage -> "remove_message"
    LogToConsole           -> "log_text"

encodeRemoveFromLocalStorage : E.Value
encodeRemoveFromLocalStorage  =
  E.object
    [
      ("eventType", E.string (showPortType RemoveFromLocalStorage))
    ]

encodeLogToConsole : String -> E.Value
encodeLogToConsole  error =
  E.object
    [
      ("eventType", E.string (showPortType LogToConsole))
    , ("output", E.string error)
    ]

encode : PortType -> N.Note -> E.Value
encode portType model =
  E.object
    [ ("eventType", E.string (showPortType portType))
    , ("noteText", E.string model.noteText)
    , ("noteId", E.int model.noteId)
    ]


encodeViewNotes : List N.Note -> E.Value
encodeViewNotes notes =
  E.object
    [ ("eventType", E.string (showPortType SaveToSessionStorage))
    , ("view_data", E.list N.encodeNote notes)
    ]


decoderJsResponseEvent: D.Decoder JsResponseEvent
decoderJsResponseEvent = D.andThen stringToJsResponseEvent decodeJsResponseString

decodeJsResponseString :D.Decoder String
decodeJsResponseString = D.field "eventType" D.string

stringToJsResponseEvent: String -> D.Decoder JsResponseEvent
stringToJsResponseEvent value =
  case value of
    "message_removed" -> D.succeed RemovedFromLocalStorage
    "message_saved"   -> D.succeed SavedToLocalStorage
    other             -> D.fail <| "Unknown JS Response type: " ++ other
