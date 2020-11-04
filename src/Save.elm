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


--
-- MAIN
--

main: Program E.Value Model Msg
main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }

--
-- MODEL
--

type alias NoteIdVersion = { noteId : Int, noteVersion : Int }

-- Where did the note originate?
type DataSource = LocalLoad
                | InitNote
                | UserCreated

type alias RemoteNoteData = WebData NoteIdVersion

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


init : E.Value -> (Model, Cmd Msg)
init json =
  let decodeResult = D.decodeValue decodeLocalSave json
  in foldResult handleInitFailure handleInitSuccess decodeResult

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


--
-- UPDATE
--

type PortType = SaveMessage
              | PreviewMessage


type Msg = NoteSavedMsg
         | NoteEditedMsg String
         | NewNoteMsg
         | ViewNoteMsg
         | NoteSaveResponseMsg RemoteNoteData


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
  --  NoteSavedMsg -> saveNote model

    (NoteEditedMsg newNoteText) ->
       let updatedModel =
            case model.note of
              BrandNewNote                          -> { model | note = HavingContent <| NoteWithoutId <| SC.NoteLight newNoteText,     noteContentStatus  = NeedsToSave }
              (HavingContent (NoteWithoutId _))     -> { model | note = HavingContent <| NoteWithoutId <| SC.NoteLight newNoteText,     noteContentStatus  = NeedsToSave }
              (HavingContent (NoteWithId fullNote))   ->
                let note =  SC.updateNoteText newNoteText fullNote
                in { model | note = HavingContent <| NoteWithId note, noteContentStatus  = NeedsToSave }
       in onlyModel updatedModel  --(updatedModel, scribMessage (encode PreviewMessage updatedModel))

    NewNoteMsg -> onlyModel { defaultModel | dataSource =  UserCreated, apiKey = model.apiKey }

    ViewNoteMsg -> (model, Browser.Navigation.load "view.html")

  --  (NoteSaveResponseMsg noteResponse) ->
  --    let updatedNote =
  --          case model.note of
  --            BrandNewNote            -> model.note -- illegal
  --            (HavingContent content) -> HavingContent <| noteFromRemoteSave content noteResponse
  --        updatedModel = {model | remoteSaveStatus = noteResponse, note = updatedNote, noteContentStatus = contentStatusFromRemoteSave noteResponse }
  --    in (updatedModel, sendSaveMessage updatedModel)
    _ -> onlyModel model



--contentStatusFromRemoteSave : RemoteNoteData -> ContentStatus
--contentStatusFromRemoteSave remoteData =
--  if RemoteData.isSuccess remoteData then UpToDate else NeedsToSave

--noteFromRemoteSave : NoteWithContent -> RemoteNoteData -> NoteWithContent
--noteFromRemoteSave existingNote remoteData =
--  case (existingNote, remoteData) of
--    (NoteWithoutId noteText, Success { noteId, noteVersion }) -> NoteWithId  { noteId = noteId, noteText = noteText, noteVersion = noteVersion }
--    (NoteWithoutId _, _) -> existingNote -- if we didn't succeed in updating the note, then there's no id to save
--    ((NoteWithId _ ), _) -> existingNote -- if we already have an id, then there's nothing to update


--saveNote: Model -> (Model, Cmd Msg)
--saveNote model =
--  case model.remoteSaveStatus of
--    Loading       -> (model, Cmd.none) -- still loading from a previous save...
--    _             ->
--      case model.note of
--        BrandNewNote -> (model, Cmd.none)
--        HavingContent content ->
--          let  (newModel, remoteSaveCmd) =  performOrGotoConfig model ({model | remoteSaveStatus = Loading }, performSaveNote content)
--          in (newModel, Cmd.batch [remoteSaveCmd, sendSaveMessage newModel]) -- TODO: Change this into a Task

--sendSaveMessage: Model -> Cmd Msg
--sendSaveMessage model = scribMessage (encode SaveMessage model)


--performSaveNote: NoteWithContent -> ApiKey -> Cmd Msg
--performSaveNote note apiKey =
--  Http.request {
--   method    = "POST"
--  , headers  = [apiKeyHeader apiKey]
--  , url      = "/note"
--  , body     = Http.jsonBody <| encodeSaveNote note
--  , expect = Http.expectJson processSaveNoteResults decoderNoteIdVersion
--  , timeout  = Nothing
--  , tracker  = Nothing
--  }

--performOrGotoConfig : Model -> (Model, (ApiKey -> Cmd Msg)) -> (Model, Cmd Msg)
--performOrGotoConfig oldModel apiKeyCommand =
--  performApiKey
--    oldModel.apiKey
--    apiKeyCommand
--    (oldModel, Browser.Navigation.load "config.html")

--processSaveNoteResults: Result Http.Error NoteIdVersion -> Msg
--processSaveNoteResults = processHttpResult NoteSaveResponseMsg

--processHttpResult: (RemoteNoteData -> Msg) -> Result Http.Error NoteIdVersion -> Msg
--processHttpResult toMsg httpResult   =
--  let result  = RemoteData.fromResult httpResult
  --in toMsg result

--
-- VIEW
--

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

--
-- PORTS
--

port scribMessage : E.Value -> Cmd msg

--
-- SUBSCRIPTIONS
--

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

--
-- JSON ENCODE/DECODE
--

--encodeSaveNote: NoteWithContent -> E.Value
--encodeSaveNote note =
--  case note of
--    (NoteWithoutId noteText)            -> encodeUnsavedNote noteText
--    (NoteWithId { noteId, noteText, noteVersion }) -> N.encodeNote { noteId = noteId, noteText = noteText, noteVersion = noteVersion }

--encodeUnsavedNote: String -> E.Value
--encodeUnsavedNote noteText =
--  E.object
--    [
--      ("noteText", E.string noteText)
--    ]


--encode : PortType -> Model -> E.Value
--encode portType model =
--  E.object
--    [
--      ("eventType", E.string (showPortType portType))
--    , ("note", encodeNoteForLocalSave model.note)
--    ]


--encodeNoteForLocalSave : Note -> E.Value
--encodeNoteForLocalSave note =
--  E.object
--    [
--      ("noteText", E.string <| getNoteText note)
--    , ("noteId", maybe E.null E.int <| getNoteId note)
--    , ("noteVersion", maybe E.null E.int <| getNoteVersion note)
--    ]


-- MODEL HELPERS


getNoteVersion : Note -> Maybe Int
getNoteVersion note = Nothing
  --case note of
  --  BrandNewNote                                 -> Nothing
  --  (HavingContent (NoteWithId { noteVersion })) -> Just noteVersion
  --  (HavingContent (NoteWithoutId _))            -> Nothing

getNoteId : Note -> Maybe Int
getNoteId note = Nothing
  --case note of
  --  BrandNewNote           -> Nothing
  --  (HavingContent (NoteWithId { noteId }))  -> Just noteId
  --  (HavingContent (NoteWithoutId _))        -> Nothing

getNoteText : Note -> String
getNoteText note =
  case note of
    BrandNewNote                                 -> ""
    (HavingContent (NoteWithId { noteText }))    -> noteText
    (HavingContent (NoteWithoutId { noteText })) -> noteText


-- DECODERS


--decoderNoteIdVersion : D.Decoder NoteIdVersion
--decoderNoteIdVersion =
--  D.map2
--    NoteIdVersion
--    (D.field "noteId" D.int)
--    (D.field "noteVersion" D.int)

-- TODO: What happens if we go directly to Save without saving an edit?
--decoderLocalEdits : D.Decoder LocalEdit
--decoderLocalEdits =
  --let
      --maybeNoteIdDecoder      = D.maybe (D.at ["note", "noteId"] D.int)
      --maybeNoteTextDecoder    = D.maybe (D.at ["note", "noteText"] D.string)
      --maybeNoteVersionDecoder = D.maybe (D.at ["note", "noteVersion"] D.int)
      --apiKeyDecoder        = D.field "apiKey" decodeApiKey
      --noteDecoder          =
        --D.map3
        --  maybeNoteContent
        --  maybeNoteIdDecoder
        --  maybeNoteTextDecoder
        --  maybeNoteVersionDecoder
  --in D.map2
  --    LocalEdit
  --    apiKeyDecoder
  --    noteDecoder


--maybeNoteContent : Maybe Int -> Maybe String -> Maybe Int -> Note
--maybeNoteContent maybeNoteId maybeNoteText maybeNoteVersion =
--  case (maybeNoteId, maybeNoteText, maybeNoteVersion) of
--    (Just noteId, Just noteText, Just noteVersion) -> HavingContent <| NoteWithId { noteId = noteId,  noteText = noteText, noteVersion = noteVersion }
--    (Just noteId, Nothing, _)                      -> BrandNewNote -- technically shouldn't happen, treat it as no note
--    (Nothing, Just noteText, _)                    -> HavingContent <| NoteWithoutId noteText -- ignore version as it's an illegal state
--    (Nothing, Nothing, _)                          -> BrandNewNote
--    (Just noteId, Just noteText, Nothing)          -> BrandNewNote -- TODO: we need to couple noteId and Version as they don't make sense separately

--
-- UTIL
--

hasContent: Note -> Bool
hasContent note = False
  --case note of
  --  BrandNewNote -> False
  --  (HavingContent (NoteWithoutId noteText))   -> not (String.isEmpty noteText)
  --  (HavingContent (NoteWithId { noteText } )) -> not (String.isEmpty noteText)


showPortType: PortType -> String
showPortType portType =
  case portType of
    SaveMessage    -> "save_message"
    PreviewMessage -> "preview_message"


fromHttpError: Http.Error -> String
fromHttpError error =
  case error of
    (Http.BadUrl burl)      -> "bad url: " ++ burl
    Http.Timeout            -> "timeout"
    Http.NetworkError       -> "network error"
    (Http.BadStatus status) -> "bad status: " ++ String.fromInt status
    (Http.BadBody body)     -> "bad body: " ++ body
