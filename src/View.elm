port module View exposing (..)

import RemoteData      exposing (..)
import Html            exposing (..)
import Html.Attributes exposing (..)
import ElmCommon       exposing (..)

import Html.Events     exposing (onClick, onInput)
import FP              exposing (maybe)
import StorageKeys     exposing (topNotesKey)
import ApiKey          exposing (ApiKey, ApiKeyWithPayload, apiKeyHeader, decodeApiKeyWithPayload)

import Debug
import Browser.Navigation
import Browser
import Http
import Browser.Navigation
import Json.Decode  as D
import Json.Encode  as E
import Note         as SC
import Ports        as PORTS
import Subs         as SUBS



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


type alias LocalNotes = { apiKey: ApiKey, notes: List SC.NoteFull }

type alias Model =
  {
    query: Maybe String
  , notes: RemoteNotesData
  , selectedNote: Maybe SC.NoteFull
  , apiKey: Maybe ApiKey
  }

type alias RemoteNotesData = WebData (List SC.NoteFull)

--type JsResponseEvent = SavedToLocalStorage
--                     | RemovedFromLocalStorage

emptyModel: Model
emptyModel = Model Nothing NotAsked Nothing Nothing

debug : String -> ()
debug message = Debug.log message ()

init : E.Value -> (Model, Cmd Msg)
init topNotes =
    let _ = debug <| "called init with: " ++ (E.encode 2 topNotes)
        decodeResult = D.decodeValue decodeLocalNotes topNotes
    in case decodeResult of
         (Ok { apiKey, payload }) ->
           let  _ = debug "decoded!!"
                notes = maybe [] identity payload
                mc    =
                  if List.isEmpty notes
                  then (
                         { emptyModel | notes = Loading, apiKey = Just apiKey }
                       , Cmd.batch [getTopRemoteNotes apiKey, logMessage <| "No cached data, refreshing"]
                       )
                  else onlyModel { emptyModel | notes = Success notes, apiKey = Just apiKey }
           in mc
         Err err       ->
            let _ = debug "failed!!"
            in (emptyModel, Browser.Navigation.load "config.html")

decodeLocalNotes : D.Decoder (ApiKeyWithPayload (List SC.NoteFull))
decodeLocalNotes = decodeApiKeyWithPayload topNotesKey SC.decodeFullNotes


-- UPDATE


getTopRemoteNotes: ApiKey -> Cmd Msg
getTopRemoteNotes apiKey =
  let _ = debug <| "calling slate"
  in
      Http.request {
       method    = "GET"
      , headers  = [apiKeyHeader apiKey]
      , url      = "/notes"
      , body     = Http.emptyBody
      , expect   = Http.expectJson (RemoteData.fromResult >> TopNotesResponse) SC.decodeFullNotes
      , timeout  = Nothing
      , tracker  = Nothing
      }

--searchRemoteNotes: String -> ApiKey -> Cmd Msg
--searchRemoteNotes query apiKey =
--  Http.request {
--   method    = "GET"
--  , headers  = [apiKeyHeader apiKey]
--  , url = "/search?q=" ++ query
--  , body     = Http.emptyBody
--  , expect = Http.expectJson (RemoteData.fromResult >> SearchNotesResponse) N.decodeNotes
--  , timeout  = Nothing
--  , tracker  = Nothing
--  }

--type SaveType = SaveResponse | DontSaveResponse

type Msg = -- NoteSelected N.Note
         --| NoteEdited N.Note
         --| NoteSavedToLocalStorage
         --| NoteRemovedFromLocalStorage
         --| JSNotificationError String
         --| AddNote
         --| SearchEdited String
         --| NotesRefreshed
         {-- | -} TopNotesResponse RemoteNotesData
         --| SearchNotesResponse RemoteNotesData

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
  --  (NoteSelected note)         -> ({model| selectedNote = Just note }, scribMessage (encode PreviewMessage note))
  --  (NoteEdited note)           -> (model, scribMessage (encode SaveToLocalStorage note))
  --  NoteSavedToLocalStorage     -> (model, Browser.Navigation.load "save.html")
  --  NoteRemovedFromLocalStorage -> (model, Browser.Navigation.load "save.html")
  --  (JSNotificationError error) -> (model, scribMessage(encodeLogToConsole error))
  --  AddNote                     -> (model, scribMessage encodeRemoveFromLocalStorage)
    (TopNotesResponse notes)    ->  onlyModel model -- ({ model | notes = notes}, logResponseErrors SaveResponse notes)
  --  (SearchNotesResponse notes) -> ({ model | notes = notes}, logResponseErrors DontSaveResponse notes)
  --  NotesRefreshed              -> performOrGotoConfig model ({ model | notes = Loading, query = Nothing }, getTopRemoteNotes)
  --  (SearchEdited query)        -> performOrGotoConfig model ({ model | query = Just query }, searchRemoteNotes query)
    --SearchPerformed             -> (model, searchRemoteNotes "")

--performOrGotoConfig : Model -> (Model, (ApiKey -> Cmd Msg)) -> (Model, Cmd Msg)
--performOrGotoConfig oldModel apiKeyCommand =
--  performApiKey
--    oldModel.apiKey
--    apiKeyCommand
--    (oldModel, Browser.Navigation.load "config.html")


logMessage: String -> Cmd Msg
logMessage = scribMessage << PORTS.encodePort << PORTS.ViewPort << PORTS.LogMessageToConsole

--logResponseErrors: SaveType -> RemoteNotesData -> Cmd Msg
--logResponseErrors saveContent remoteData =
--  case (saveContent, remoteData) of
--    (_, Failure e)                -> scribMessage <| encodeLogToConsole <| fromHttpError e
--    (SaveResponse, Success notes) -> scribMessage <| encodeViewNotes notes
--    (DontSaveResponse, Success _) -> Cmd.none
--    (_, _)                        -> Cmd.none


-- VIEW


view : Model -> Html Msg
view model = div [] []
  --div []
  --  [ section [ class "section" ]
  --    [ div [ class "container" ]
  --      [ h1 [ class "title" ]
  --        [ text "Scrib" ]
  --      , p [ class "subtitle" ]
  --        [ text "Making scribbling effortless" ]
  --      , div []
  --        [ article [ class "panel", class "is-primary" ]
  --          [ p [ class "panel-heading" ]
  --            [ text "Saved Notes"
  --            , text " "
  --            , span [class "tab", class "is-medium"] [text <| getNoteCount model.notes]
  --            ]
  --          , p [ class "panel-tabs" ]
  --            [ button [ class "button", class "is-text", onClick AddNote]
  --              [ text "Add Note" ]
  --            , button [ class "button", class "is-text", onClick NotesRefreshed ]
  --              [ text "Refresh" ]
  --            ]
  --          , div [ class "panel-block" ]
  --            [ p [ class "control has-icons-left" ]
  --              [ input [ class "input", class "is-primary", placeholder "Search", type_ "text", onInput SearchEdited, value <| getQueryText model.query ]
  --                []
  --              , span [ class "icon is-left" ]
  --                [ i [ attribute "aria-hidden" "true", class "fas", class "fa-search" ]
  --                  []
  --                ]
  --              ]
  --            ]
  --          , viewNotesList model.notes

  --          ]
  --        ]
  --      ]
  --    ]
  -- , createEditButton model.selectedNote
  -- ]

--getQueryText : Maybe String -> String
--getQueryText = maybe "" identity

--getNoteCount: RemoteNotesData -> String
--getNoteCount remoteNoteData =
--  case remoteNoteData of
--    Success notes -> String.fromInt <| List.length notes
--    _             -> "-"

--viewNotesList: RemoteNotesData -> Html Msg
--viewNotesList remoteNotesData =
--  let notesContent =
--        case remoteNotesData of
--          NotAsked      -> [div [] [text "No Data"]]
--          Loading       -> [div [] [text "Loading..."]]
--          Failure e     -> [addFailureAlert <| "oops! Could not get your data :(" ++ fromHttpError e]
--          Success notes -> List.map createNoteItem notes
--  in div [ id "notes-list" ] notesContent

--fromHttpError: Http.Error -> String
--fromHttpError error =
--  case error of
--    (Http.BadUrl burl)      -> "bad url: " ++ burl
--    Http.Timeout            -> "timeout"
--    Http.NetworkError       -> "network error"
--    (Http.BadStatus status) -> "bad status: " ++ String.fromInt status
--    (Http.BadBody body)     -> "bad body: " ++ body


--createEditButton: Maybe N.Note -> Html Msg
--createEditButton = maybe viewMarkdownPreviewDefault viewMarkdownPreview

---- Update this to only create a note with the text for the first line
--createNoteItem: N.Note -> Html Msg
--createNoteItem {noteText, noteId, noteVersion } =
--  a [class "panel-block", onClick (NoteSelected { noteText = noteText, noteId = noteId, noteVersion = noteVersion })]
--  [ span [class "panel-icon"]
--    [ i [ class "fas", class "fa-book", attribute "aria-hidden" "true"]
--      []
--    ]
--  , text <| removeHeading <| onlyHeading noteText
--   ]

--onlyHeading : String -> String
--onlyHeading note = maybe "-no title-" identity (List.head <| String.split "\n" note)

--removeHeading : String -> String
--removeHeading = String.replace "# " ""

--viewMarkdownPreview : N.Note -> Html Msg
--viewMarkdownPreview note =
--  div []
--    [ hr []
--      []
--    , div [ id "preview" ]
--      [ div [ id "markdown-view" ]
--        []
--      , button [ class "button", class "is-info", onClick (NoteEdited note) ]
--        [ text "Edit" ]
--      ]
--    ]

--viewMarkdownPreviewDefault: Html Msg
--viewMarkdownPreviewDefault =
--  div []
--    [ hr []
--      []
--    , div [ id "preview" ]
--      [ div [ id "markdown-view" ]
--        []
--      ]
--    ]

-- PORTS

port scribMessage : E.Value -> Cmd msg
port jsMessage : (E.Value -> msg) -> Sub msg

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =  Sub.none --jsMessage << S.handleJSResponse subscriptionSuccess subscriptionFailure


--subscriptionSuccess : S.SubType -> Msg
--subscriptionSuccess (S.ViewSub response) =
--  case response of
--    S.NoteSavedToLocalStorage     -> NoteSavedToLocalStorage
--    S.NoteRemovedFromLocalStorage -> NoteRemovedFromLocalStorage

--subscriptionFailure : String -> Msg
--subscriptionFailure = JSNotificationError