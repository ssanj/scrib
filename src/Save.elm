port module Save exposing (..)

import Html            exposing (..)
import RemoteData      exposing (..)
import Html.Attributes exposing (..)
import ElmCommon       exposing (..)
import StorageKeys     exposing (..)
import Notifications   exposing (..)

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

type WhatAreWeDoing = SavingNoteRemotely
                    | Idle

type alias RemoteNoteData = WebData SC.NoteIdVersion

type NoteWithContent = NoteWithoutId SC.NoteLight
                     | NoteWithId SC.NoteFull

type ContentStatus = NeedsToSave
                   | UpToDate

type alias Model =
  {
    note: NoteWithContent
  , dataSource: DataSource
  , remoteSaveStatus: RemoteNoteData
  , noteContentStatus: ContentStatus
  , apiKey: Maybe ApiKey
  , successMessage : Maybe SuccessMessage
  , doing : WhatAreWeDoing
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
         --| InlineInfoTimedOut

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
    NoteSavedMsg                           -> handleSavingNote model
    (NoteEditedMsg newNoteText)            -> handleEditingNote model newNoteText
    NewNoteMsg                             -> handleNewNote model
    ViewNoteMsg                            -> handleGoingToView model
    (NoteSaveResponseMsg noteResponse)     -> handleNoteSaveResponse model noteResponse
    NoteSavedToLocalStorage                -> handleRemoteSave model
    RemoteNoteIdVersionSavedToLocalStorage -> handleNoteIdVersionSavedToLocalStorage model
    (JSNotificationError error)            -> handleJSError model error

    --InlineInfoTimedOut -> handleInlineTimeout model

--successMessageLens : Lens Model (Maybe SuccessMessage)
--successMessageLens = { getter = .successMessage, setter = \m newMessage -> { m | successMessage = newMessage } }

--handleInlineTimeout : Model -> (Model, Cmd Msg)
--handleInlineTimeout model = onlyModel <| onInlineTimeout successMessageLens model

--handleSuccessMessage : Model -> SuccessMessage -> (Model, Cmd Msg)
--handleSuccessMessage model message seconds =
--  addInlineMessage (getSetter successMessageLens) model message inlineInfoSuccessTimeout InlineInfoTimedOut

--timeoutInlineSuccessMessage : Cmd Msg
--timeoutInlineSuccessMessage = addTimeoutForInlineMessage inlineInfoSuccessTimeout InlineInfoTimedOut

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


decodeLocalSave : D.Decoder (ApiKeyWithPayload NoteWithContent)
decodeLocalSave = decodeApiKeyWithPayload noteKey decodeNoteWithContent

handleInitSuccess : ApiKeyWithPayload NoteWithContent -> (Model, Cmd Msg)
handleInitSuccess { apiKey, payload } =
    let note  = maybe (NoteWithoutId <| SC.mkLightNote "") identity payload
        model =
            { defaultModel | note = note, apiKey = Just apiKey, dataSource = LocalLoad }
    in onlyModel model



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

defaultModel: Model
defaultModel = Model defaultNote InitNote NotAsked UpToDate Nothing Nothing Idle

defaultNote : NoteWithContent
defaultNote = NoteWithoutId <| SC.mkLightNote ""


getNoteVersion : NoteWithContent -> Maybe Int
getNoteVersion note =
  case note of
    NoteWithId scNote -> Just <| SC.getNoteFullVersion scNote
    NoteWithoutId _   -> Nothing

getNoteId : NoteWithContent -> Maybe Int
getNoteId note =
  case note of
    NoteWithId scNote -> Just <| SC.getNoteFullId scNote
    NoteWithoutId _   -> Nothing

getNoteText : NoteWithContent -> String
getNoteText note =
  case note of
    NoteWithId scNote    -> SC.getNoteFullText scNote
    NoteWithoutId scNote -> SC.getNoteLightText scNote

hasContent: NoteWithContent -> Bool
hasContent note =
  case note of
    NoteWithoutId noteText -> not (String.isEmpty <| SC.getNoteLightText noteText)
    NoteWithId noteText    -> not (String.isEmpty <| SC.getNoteFullText noteText)

-- TODO: Put the constants in one place
inlineInfoSuccessTimeout : Seconds
inlineInfoSuccessTimeout = Seconds 1

-- REMOTE HELPERS


--contentStatusFromRemoteSave : RemoteNoteData -> ContentStatus
--contentStatusFromRemoteSave remoteData =
--  if RemoteData.isSuccess remoteData then UpToDate else NeedsToSave

--processRemoteSave : Model -> RemoteNoteData -> Model
--processRemoteSave model remoteData =
--  case remoteData of
--    Success _ -> { model | successMessage = Just <| SuccessMessage "Saved note" }
    --_         -> model

--noteFromRemoteSave : NoteWithContent -> RemoteNoteData -> NoteWithContent
--noteFromRemoteSave existingNote remoteData =
--  case (existingNote, remoteData) of
--    (NoteWithoutId noteText, Success noteIdVersion) -> NoteWithId <| SC.updateNoteIdVersion noteIdVersion noteText
--    (NoteWithoutId _, _)                            -> existingNote -- if we didn't succeed in updating the note, then there's no id to save
--    ((NoteWithId fullNote), Success noteIdVersion)  -> NoteWithId <| SC.updateNoteVersion noteIdVersion fullNote -- if we already have an id, then there's nothing to update
--    ((NoteWithId fullNote), _)                      -> existingNote -- if we didn't succeed then don't update the existing note

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


handleSavingNote: Model -> (Model, Cmd Msg)
handleSavingNote model =
  let saveNoteToLocalCmd = saveEditingNoteToLocalStorage noteSavedToLocalStorageResponseKey model.note
      newModel           = { model | doing = SavingNoteRemotely, remoteSaveStatus = Loading }
  in (newModel, saveNoteToLocalCmd)

handleRemoteSave : Model -> (Model, Cmd Msg)
handleRemoteSave model =
  performOrGotoConfig model ({model | remoteSaveStatus = Loading, doing = SavingNoteRemotely }, performSaveNote model.note)

handleJSError : Model -> String -> (Model, Cmd Msg)
handleJSError model error = (model, logMessage error)

handleNoteIdVersionSavedToLocalStorage : Model -> (Model, Cmd Msg)
handleNoteIdVersionSavedToLocalStorage = onlyModel

handleEditingNote : Model -> String -> (Model, Cmd Msg)
handleEditingNote model newNoteText =
  let updatedModel =
        case model.note of
          (NoteWithoutId _)     -> { model | note = NoteWithoutId <| SC.updateNoteLightText newNoteText, noteContentStatus  = NeedsToSave }
          (NoteWithId fullNote) ->
            let note =  SC.updateNoteFullText newNoteText fullNote
            in { model | note = NoteWithId note, noteContentStatus  = NeedsToSave }
  in onlyModel updatedModel

handleNewNote : Model -> (Model, Cmd Msg)
handleNewNote model =
  onlyModel {
    defaultModel |
      dataSource       = UserCreated
    , apiKey           = model.apiKey
    , remoteSaveStatus = NotAsked
    , doing            = Idle
  }

handleGoingToView : Model -> (Model, Cmd Msg)
handleGoingToView model = (model, Browser.Navigation.load "view.html")

-- RemoteNoteData should only give us a Success or Failure
handleNoteSaveResponse : Model -> RemoteNoteData -> (Model, Cmd Msg)
handleNoteSaveResponse model remoteData =
  case remoteData of
    (Success noteIdVersion) ->
      let newNote =
            case model.note of
              NoteWithoutId noteText -> NoteWithId <| SC.updateNoteIdVersion noteIdVersion noteText
              NoteWithId fullNote    -> NoteWithId <| SC.updateNoteVersion noteIdVersion fullNote
      in (
            {
              model |
                note              = newNote
              , remoteSaveStatus  = remoteData
              , doing             = Idle
              , noteContentStatus = UpToDate
            }
            , saveRemoteUpdateToLocalStorage newNote
         )

    (Failure _)             ->
      -- TODO: We should set AppErrors here
      onlyModel
        {
          model |
            remoteSaveStatus = remoteData
          , doing = Idle
          , noteContentStatus = NeedsToSave
        }

    -- these two don't make any sense at this point
    NotAsked                -> onlyModel { model | remoteSaveStatus = remoteData, doing = Idle }
    Loading                 -> onlyModel { model | remoteSaveStatus = remoteData, doing = SavingNoteRemotely }


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
      viewInlineInfoIfAny model.doing
    , viewInlineSuccessIfAny model.successMessage
    --, viewInlineErrorIfAny model.appErrors
    , viewNotesTextArea model.note
    , viewControls model -- TODO: Fix
    ]


viewInlineInfoIfAny : WhatAreWeDoing -> Html a
viewInlineInfoIfAny doing =
  case doing of
    Idle               -> emptyDiv
    SavingNoteRemotely -> addInlineInfoFlash <| InformationMessage "Saving..."

viewInlineSuccessIfAny : Maybe SuccessMessage -> Html a
viewInlineSuccessIfAny = maybe emptyDiv addInlineSuccessFlash

--viewNotificationsArea: RemoteNoteData -> Html a
--viewNotificationsArea remoteSaveStatus =
--  case remoteSaveStatus of
--    Failure e   -> addInlineErrorFlash <| ErrorMessage <| "Save failed: " ++ fromHttpError e -- show error
--    (Success _) -> addInlineSuccessFlash <| SuccessMessage "Saved note"
--    _           -> hideAlertSpace


viewNotesTextArea: NoteWithContent -> Html Msg
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


createMarkdownPreview : NoteWithContent -> Html Msg
createMarkdownPreview note =
  case note of
    (NoteWithoutId scNote) ->
      let noteText = SC.getNoteLightText scNote
      in
        if String.isEmpty noteText then viewMarkdownInstructions
        else viewMarkdownPreview noteText

    (NoteWithId scNote)    ->
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

saveRemoteUpdateToLocalStorage : NoteWithContent -> Cmd Msg
saveRemoteUpdateToLocalStorage note =
  saveEditingNoteToLocalStorage remoteNoteIdVersionSavedToLocalStorageResponseKey note

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
    NoteWithoutId lightNote -> SC.encodeLightNote lightNote
    NoteWithId fullNote     -> SC.encodeFullNote fullNote


-- DECODERS


decodeNoteWithContent : D.Decoder NoteWithContent
decodeNoteWithContent =
  D.oneOf
  [
    D.map NoteWithId SC.decodeFullNote
  , D.map NoteWithoutId SC.decodeLightNote
  ]