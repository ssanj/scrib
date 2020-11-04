port module Save exposing (..)

import Html            exposing (..)
import RemoteData      exposing (..)
import Html.Attributes exposing (..)
import ElmCommon       exposing (..)
import StorageKeys     exposing (..)

import ApiKey          exposing (ApiKey, ApiKeyWithPayload, decodeApiKey, apiKeyHeader, decodeApiKeyWithPayload, performApiKey)
import Html.Events     exposing (onClick, onInput)
import FP              exposing (maybe, const)

import Browser
import Http
import Browser.Navigation
import Markdown

import List.Nonempty as N
import Json.Decode   as D
import Json.Encode   as E
import Note          as SC
import Ports         as P
import Subs          as S


-- MODEL


-- Where did the note originate?
type DataSource = LocalLoad
                | InitNote
                | UserCreated

type alias RemoteNoteData = WebData SC.NoteIdVersion

type NoteWithContent = NoteWithoutId SC.NoteLight
                     | NoteWithId SC.NoteFull

-- What information do we have about the note?
type Note = BrandNewNote
          | HavingContent NoteWithContent

type ContentStatus = NeedsToSave
                   | UpToDate

type alias LocalEdit = { apiKey: ApiKey, note: Note }

type alias Model =
  {
    note: Note
  , dataSource: DataSource
  , remoteSaveStatus: RemoteNoteData
  , noteContentStatus: ContentStatus
  , apiKey: Maybe ApiKey
  }


type PortType = SaveMessage
              | PreviewMessage


type Msg = NoteSavedMsg
         | NoteEditedMsg String
         | NewNoteMsg
         | ViewNoteMsg
         | NoteSaveResponseMsg RemoteNoteData
         | NoteSavedToLocalStorage
         | RemoteNoteIdVersionSavedToLocalStorage
         | JSNotificationError String

-- MAIN


main: Program E.Value Model Msg
main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }


-- INIT

init : E.Value -> (Model, Cmd Msg)
init json =
  let decodeResult = D.decodeValue decodeLocalSave json
  in foldResult handleInitFailure handleInitSuccess decodeResult



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoteSavedMsg -> saveNote model

    (NoteEditedMsg newNoteText) ->
       let updatedModel =
            case model.note of
              BrandNewNote                          -> { model | note = HavingContent <| NoteWithoutId <| SC.NoteLight newNoteText,     noteContentStatus  = NeedsToSave }
              (HavingContent (NoteWithoutId _))     -> { model | note = HavingContent <| NoteWithoutId <| SC.NoteLight newNoteText,     noteContentStatus  = NeedsToSave }
              (HavingContent (NoteWithId fullNote))   ->
                let note =  SC.updateNoteText newNoteText fullNote
                in { model | note = HavingContent <| NoteWithId note, noteContentStatus  = NeedsToSave }
       in onlyModel updatedModel

    NewNoteMsg -> onlyModel { defaultModel | dataSource =  UserCreated, apiKey = model.apiKey }

    ViewNoteMsg -> (model, Browser.Navigation.load "view.html")

    (NoteSaveResponseMsg noteResponse) ->
      let updatedNote =
            case model.note of
              BrandNewNote            -> model.note -- illegal, you shouldn't be able to save a new note (without content)
              (HavingContent content) -> HavingContent <| noteFromRemoteSave content noteResponse
          updatedModel = {model | remoteSaveStatus = noteResponse, note = updatedNote, noteContentStatus = contentStatusFromRemoteSave noteResponse }
      in (updatedModel, saveRemoteUpdateToLocalStorage updatedModel.note)

    NoteSavedToLocalStorage ->
      case model.note of
        BrandNewNote -> onlyModel model
        (HavingContent content) -> performOrGotoConfig model ({model | remoteSaveStatus = Loading }, performSaveNote content)

    RemoteNoteIdVersionSavedToLocalStorage -> onlyModel model

    (JSNotificationError error) -> (model, logMessage error)


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
          , createMarkdownPreview model.note
          ]
        )
    ]


-- PORTS


port scribMessage : E.Value -> Cmd msg
port jsMessage : (E.Value -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = jsMessage (S.encodeJsResponse subscriptionSuccess subscriptionFailure)


appName : String
appName = "scrib"

appMessage : a -> P.JsAppMessage a
appMessage = P.JsAppMessage appName

logMessage: String -> Cmd Msg
logMessage message =
  let logCommand = P.LogConsole <| appMessage message
  in scribMessage <| P.encodeJsCommand logCommand E.string


decodeLocalSave : D.Decoder (ApiKeyWithPayload SC.Note)
decodeLocalSave = decodeApiKeyWithPayload noteKey SC.decodeNote

handleInitSuccess : ApiKeyWithPayload SC.Note -> (Model, Cmd Msg)
handleInitSuccess { apiKey, payload } =
    let note  = maybe BrandNewNote (HavingContent << createNote) payload
        model =
            { defaultModel | note = note, apiKey = Just apiKey, dataSource = LocalLoad }
    in onlyModel model


createNote : SC.Note -> NoteWithContent
createNote scNote =
  case scNote of
    (SC.Note noteFull)      -> NoteWithId noteFull
    (SC.NoteText noteLight) -> NoteWithoutId noteLight

-- TODO: Should we have a FATAL error instead of logging to the console and redirecting to config?
handleInitFailure : D.Error -> (Model, Cmd Msg)
handleInitFailure err = (
                        defaultModel
                      , Cmd.batch
                        [
                          Browser.Navigation.load "config.html"
                        , logMessage ("Decode of init data failed due to: " ++ D.errorToString err)
                        ]
                      )

defaultModel: Model
defaultModel = Model BrandNewNote InitNote NotAsked UpToDate Nothing


-- REMOTE CALLS


performSaveNote: NoteWithContent -> ApiKey -> Cmd Msg
performSaveNote note apiKey =
  Http.request {
   method    = "POST"
  , headers  = [ apiKeyHeader apiKey ]
  , url      = "/note"
  , body     = Http.jsonBody <| encodeSaveNote note
  , expect   = Http.expectJson processSaveNoteResults SC.decoderNoteIdVersion
  , timeout  = Nothing
  , tracker  = Nothing
  }


-- PAGE REDIRECTS


performOrGotoConfig : Model -> (Model, (ApiKey -> Cmd Msg)) -> (Model, Cmd Msg)
performOrGotoConfig oldModel apiKeyCommand =
  performApiKey
    oldModel.apiKey
    apiKeyCommand
    (oldModel, Browser.Navigation.load "config.html")


-- MODEL HELPERS


getNoteVersion : Note -> Maybe Int
getNoteVersion note =
  case note of
    BrandNewNote                        -> Nothing
    (HavingContent (NoteWithId scNote)) -> Just <| SC.getNoteVersionNoteFull scNote
    (HavingContent (NoteWithoutId _))   -> Nothing

getNoteId : Note -> Maybe Int
getNoteId note =
  case note of
    BrandNewNote                        -> Nothing
    (HavingContent (NoteWithId scNote)) -> Just <| SC.getNoteIdNoteFull scNote
    (HavingContent (NoteWithoutId _))   -> Nothing

getNoteText : Note -> String
getNoteText note =
  case note of
    BrandNewNote                          -> ""
    (HavingContent (NoteWithId scNote))    -> SC.getNoteFullText scNote
    (HavingContent (NoteWithoutId scNote)) -> SC.getNoteLightText scNote

hasContent: Note -> Bool
hasContent note =
  case note of
    BrandNewNote                             -> False
    (HavingContent (NoteWithoutId noteText)) -> not (String.isEmpty <| SC.getNoteLightText noteText)
    (HavingContent (NoteWithId noteText))    -> not (String.isEmpty <| SC.getNoteFullText noteText)


-- REMOTE HELPERS


contentStatusFromRemoteSave : RemoteNoteData -> ContentStatus
contentStatusFromRemoteSave remoteData =
  if RemoteData.isSuccess remoteData then UpToDate else NeedsToSave

noteFromRemoteSave : NoteWithContent -> RemoteNoteData -> NoteWithContent
noteFromRemoteSave existingNote remoteData =
  case (existingNote, remoteData) of
    (NoteWithoutId noteText, Success noteIdVersion) -> NoteWithId <| SC.updateNoteIdVersion noteIdVersion noteText
    (NoteWithoutId _, _)                            -> existingNote -- if we didn't succeed in updating the note, then there's no id to save
    ((NoteWithId fullNote), Success noteIdVersion)  -> NoteWithId <| SC.updateNoteVersion noteIdVersion fullNote -- if we already have an id, then there's nothing to update
    ((NoteWithId fullNote), _)                      -> existingNote -- if we didn't succeed then don't update the existing note

processSaveNoteResults: Result Http.Error SC.NoteIdVersion -> Msg
processSaveNoteResults = processHttpResult NoteSaveResponseMsg

processHttpResult: (RemoteNoteData -> Msg) -> Result Http.Error SC.NoteIdVersion -> Msg
processHttpResult toMsg httpResult   =
  let result  = RemoteData.fromResult httpResult
  in toMsg result

fromHttpError: Http.Error -> String
fromHttpError error =
  case error of
    (Http.BadUrl burl)      -> "bad url: " ++ burl
    Http.Timeout            -> "timeout"
    Http.NetworkError       -> "network error"
    (Http.BadStatus status) -> "bad status: " ++ String.fromInt status
    (Http.BadBody body)     -> "bad body: " ++ body

-- UPDATE HELPERS


saveNote: Model -> (Model, Cmd Msg)
saveNote model =
  case model.remoteSaveStatus of
    Loading       -> (model, Cmd.none) -- still saving from a previous save...
    _             ->
      case model.note of
        BrandNewNote -> (model, Cmd.none)
        HavingContent content -> (model, saveEditingNoteToLocalStorage noteSavedToLocalStorageResponseKey content)


-- VIEW HELPERS


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
      viewNotificationsArea model.remoteSaveStatus
    , viewNotesTextArea model.note
    , viewControls model -- TODO: Fix
    ]


viewNotificationsArea: RemoteNoteData -> Html a
viewNotificationsArea remoteSaveStatus =
  case remoteSaveStatus of
    Failure e   -> addInlineErrorFlash <| ErrorMessage <| "Save failed: " ++ fromHttpError e -- show error
    (Success _) -> addInlineSuccessFlash <| SuccessMessage "Saved note"
    _           -> hideAlertSpace


viewNotesTextArea: Note -> Html Msg
viewNotesTextArea note =
  textarea
    [id "note-content", class "textarea", rows 10, placeholder "e.g. My awesome idea", onInput NoteEditedMsg, value <| getNoteText note]
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
          , onClick NewNoteMsg
          , classList
              [ ("button", True), ("is-text", True) ]
          ]
            [ text "New Note"]
        ]
        , button [ id "view-notes-button", class "button", class "is-text", onClick ViewNoteMsg ]
            [ text "View Notes" ]
        , modifiedTag model.noteContentStatus

    ]

modifiedTag : ContentStatus -> Html a
modifiedTag contentStatus =
  case contentStatus of
    UpToDate    ->
        span
          (addClasses ["tag", "is-success"])
          [ text "+" ]
    NeedsToSave    ->
        span
          (addClasses ["tag", "is-info"])
          [ text "*" ]

viewSaveButton: Model -> Html Msg
viewSaveButton model =
  let showSpinner =
        case model.remoteSaveStatus of
          Loading -> True
          _       -> False
  in button [
       id "save-note"
       , onClick NoteSavedMsg
       , classList
           [
             ("button", True)
           , ("is-success", True)
           , ("is-static", not (hasContent model.note))
           , ("is-loading", showSpinner)
           ]
     ]
      [text "Save"]


createMarkdownPreview : Note -> Html Msg
createMarkdownPreview note =
  case note of
    BrandNewNote                           -> viewMarkdownInstructions

    (HavingContent (NoteWithoutId scNote)) ->
      let noteText = SC.getNoteLightText scNote
      in
        if String.isEmpty noteText then viewMarkdownInstructions
        else viewMarkdownPreview noteText

    (HavingContent (NoteWithId scNote))    ->
      let noteText = SC.getNoteFullText scNote
      in
        if String.isEmpty noteText then viewMarkdownInstructions
        else viewMarkdownPreview noteText

viewMarkdownInstructions : Html Msg
viewMarkdownInstructions =
  div [ class "content", class "is-large my-6" ]
    [
      div [ class "block", class "has-text-centered", class "is-size-3" ]
        [ text "Please enter some text to see a preview here" ]
    ]

viewMarkdownPreview : String -> Html Msg
viewMarkdownPreview noteText =
  div []
    [ hr []
      []
    , div [ id "preview" ]
      [ div [ id markdownViewId ]
        [ Markdown.toHtml [] noteText ]
      ]
    ]

markdownViewId : String
markdownViewId = "markdown-view"


-- JS COMMANDS


noteSavedToLocalStorageResponseKey : P.ResponseKey
noteSavedToLocalStorageResponseKey = P.ResponseKey "NoteSavedToLocalStorage"

remoteNoteIdVersionSavedToLocalStorageResponseKey : P.ResponseKey
remoteNoteIdVersionSavedToLocalStorageResponseKey = P.ResponseKey "RemoteNoteIdVersionSavedToLocalStorage"

saveRemoteUpdateToLocalStorage : Note -> Cmd Msg
saveRemoteUpdateToLocalStorage note =
  case note of
    BrandNewNote -> Cmd.none
    HavingContent content -> saveEditingNoteToLocalStorage remoteNoteIdVersionSavedToLocalStorageResponseKey content

saveEditingNoteToLocalStorage : P.ResponseKey -> NoteWithContent -> Cmd Msg
saveEditingNoteToLocalStorage responseKey note =
  let storageArea             = savedNoteStorageArea
      saveSelectedNoteValue   = P.JsStorageValue storageArea Save note
      saveSelectedNoteCommand = P.WithStorage saveSelectedNoteValue (Just responseKey)
  in scribMessage <| P.encodeJsCommand saveSelectedNoteCommand encodeSaveNote



-- SUBSCRIPTION HELPERS


subscriptionSuccess : S.JsResponse E.Value -> Msg
subscriptionSuccess (S.JsResponse (P.ResponseKey key) result) =
  case (key) of
    "NoteSavedToLocalStorage"                -> NoteSavedToLocalStorage
    "RemoteNoteIdVersionSavedToLocalStorage" -> RemoteNoteIdVersionSavedToLocalStorage
    otherKey                                 -> subscriptionFailure <| ("Unhandled JS notification: " ++ otherKey)

subscriptionFailure : String -> Msg
subscriptionFailure m = JSNotificationError ("subscriptionFailure: " ++ m)


-- ENCODERS


encodeSaveNote: NoteWithContent -> E.Value
encodeSaveNote note =
  case note of
    (NoteWithoutId noteText)                       -> SC.encodeLightNote noteText
    (NoteWithId { noteId, noteText, noteVersion }) -> SC.encodeFullNote { noteId = noteId, noteText = noteText, noteVersion = noteVersion }

