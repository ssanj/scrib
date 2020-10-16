port module View exposing (..)

import RemoteData exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import ElmCommon exposing (..)

import Html.Events exposing (onClick, onInput)
import FP exposing (maybe)
import Browser.Navigation

import Browser
import Http
import Browser.Navigation

import Json.Decode as D
import Json.Encode as E
import Note        as N


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

type alias ApiKey = { value: String }

type alias LocalNotes = { apiKey: ApiKey, notes: List N.Note }

type alias Model =
  {
    query: Maybe String -- TODO: We are not using this at the moment. Consider removing it.
  , notes: RemoteNotesData
  , selectedNote: Maybe N.Note
  , apiKey: Maybe ApiKey -- How can I make this mandatory? An ADT of models?
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
emptyModel = Model Nothing NotAsked Nothing Nothing


init : E.Value -> (Model, Cmd Msg)
init localNotes =
    let decodeResult = D.decodeValue decodeLocalNotes localNotes
    in case decodeResult of
         (Ok {apiKey, notes}) ->
           let mc =
                if List.isEmpty notes
                  then (
                         { emptyModel | notes = Loading, apiKey = Just apiKey }
                       , Cmd.batch [getTopRemoteNotes apiKey, logMessage <| "No cached data, refreshing"]
                       )
                  else onlyModel { emptyModel | notes = Success notes, apiKey = Just apiKey }
           in mc
         Err err       -> (
                            emptyModel, Browser.Navigation.load "config.html"
                          )

-- UPDATE

apiKeyHeader: ApiKey -> Http.Header
apiKeyHeader apiKey = Http.header "X-API-KEY" apiKey.value



getTopRemoteNotes: ApiKey -> Cmd Msg
getTopRemoteNotes apiKey =
  Http.request {
   method    = "GET"
  , headers  = [apiKeyHeader apiKey]
  , url      = "http://localhost:3000/notes"
  , body     = Http.emptyBody
  , expect   = Http.expectJson (RemoteData.fromResult >> TopNotesResponse) N.decodeNotes
  , timeout  = Nothing
  , tracker  = Nothing
  }


  --Http.get {
  --  url = "http://localhost:3000/notes"
  --, expect = Http.expectJson (RemoteData.fromResult >> TopNotesResponse) N.decodeNotes
  --}

searchRemoteNotes: ApiKey -> String -> Cmd Msg
searchRemoteNotes apiKey query =
  Http.request {
   method    = "GET"
  , headers  = [apiKeyHeader apiKey]
  , url = "http://localhost:3000/search?q=" ++ query
  , body     = Http.emptyBody
  , expect = Http.expectJson (RemoteData.fromResult >> SearchNotesResponse) N.decodeNotes
  , timeout  = Nothing
  , tracker  = Nothing
  }

type SaveType = SaveResponse | DontSaveResponse

type Msg = NoteSelected N.Note
         | NoteEdited N.Note
         | NoteSavedToLocalStorage
         | NoteRemovedFromLocalStorage
         | JSNotificationError String
         | AddNote
         | SearchEdited String
         --| SearchPerformed
         | NotesRefreshed
         | TopNotesResponse RemoteNotesData
         | SearchNotesResponse RemoteNotesData



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    (NoteSelected note)         -> ({model| selectedNote = Just note }, scribMessage (encode PreviewMessage note))
    (NoteEdited note)           -> (model, scribMessage (encode SaveToLocalStorage note))
    NoteSavedToLocalStorage     -> (model, Browser.Navigation.load "save.html")
    NoteRemovedFromLocalStorage -> (model, Browser.Navigation.load "save.html")
    (JSNotificationError error) -> (model, scribMessage(encodeLogToConsole error))
    AddNote                     -> (model, scribMessage encodeRemoveFromLocalStorage)
    (TopNotesResponse notes)    -> ({ model | notes = notes}, logResponseErrors SaveResponse notes)
    (SearchNotesResponse notes) -> ({ model | notes = notes}, logResponseErrors DontSaveResponse notes)
    NotesRefreshed              ->
      case model.apiKey of
        Just apiKey ->  ({ model | notes = Loading }, getTopRemoteNotes apiKey)
        Nothing     ->  ({ model | notes = Loading }, Browser.Navigation.load "config.html")

    (SearchEdited query)        ->
      case model.apiKey of
        Just apiKey -> (model, searchRemoteNotes apiKey query)
        Nothing     -> ({ model | notes = Loading }, Browser.Navigation.load "config.html")

    --SearchPerformed             -> (model, searchRemoteNotes "")


logMessage: String -> Cmd Msg
logMessage = scribMessage << encodeLogToConsole

logResponseErrors: SaveType -> RemoteNotesData -> Cmd Msg
logResponseErrors saveContent remoteData =
  case (saveContent, remoteData) of
    (_, Failure e)                -> scribMessage <| encodeLogToConsole <| fromHttpError e
    (SaveResponse, Success notes) -> scribMessage <| encodeViewNotes notes
    (DontSaveResponse, Success _) -> Cmd.none
    (_, _)                        -> Cmd.none


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
              , button [ class "button", class "is-text", onClick NotesRefreshed ]
                [ text "Refresh" ]
              ]
            , div [ class "panel-block" ]
              [ p [ class "control has-icons-left" ]
                [ input [ class "input", class "is-primary", placeholder "Search", type_ "text", onInput SearchEdited ]
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
          Failure e     -> [addFailureAlert <| "oops! Could not get your data :(" ++ fromHttpError e]
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

-- TODO: Create an encapsulating type for a Note + Port Type
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


decodeApiKey : D.Decoder ApiKey
decodeApiKey = D.map ApiKey D.string

decodeLocalNotes : D.Decoder LocalNotes
decodeLocalNotes  =
  D.map2
    LocalNotes
    (D.field "apiKey" decodeApiKey)
    (D.field "notes" N.decodeNotes)


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
