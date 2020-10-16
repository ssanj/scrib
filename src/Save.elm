port module Save exposing (..)

import Html exposing (..)
import RemoteData exposing (..)
import Html.Attributes exposing (..)
import ElmCommon exposing (..)
import ApiKey exposing (..)

import Html.Events exposing (onClick, onInput)
import FP exposing (maybe, const)

import Browser
import Http
import Browser.Navigation


import Json.Decode as D
import Json.Encode as E
import Note        as N

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

-- Where did the note originate?
type DataSource = LocalLoad
                | InitNote
                | UserCreated

type alias RemoteNoteData = WebData Int

type NoteWithContent = NoteWithoutId String
                     | NoteWithId Int String

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
  let localEdit = D.decodeValue decoderLocalEdits json
  in case localEdit of
    (Ok {apiKey, note})  ->
      let model =
            {defaultModel | note = note, apiKey = Just apiKey, dataSource = LocalLoad}
      in (model, scribMessage <| encode PreviewMessage model)
    Err _     -> (defaultModel, Browser.Navigation.load "config.html")


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
    NoteSavedMsg -> saveNote model

    (NoteEditedMsg newNoteText) ->
       let updatedModel =
            case model.note of
              BrandNewNote                          -> { model | note = HavingContent <| NoteWithoutId newNoteText,     noteContentStatus  = NeedsToSave }
              (HavingContent (NoteWithoutId _))     -> { model | note = HavingContent <| NoteWithoutId newNoteText,     noteContentStatus  = NeedsToSave }
              (HavingContent (NoteWithId noteId _)) -> { model | note = HavingContent <| NoteWithId noteId newNoteText, noteContentStatus  = NeedsToSave }
       in (updatedModel, scribMessage (encode PreviewMessage updatedModel))

    NewNoteMsg ->
      let updatedModel = { defaultModel | dataSource =  UserCreated }
      in (updatedModel, scribMessage (encode PreviewMessage updatedModel)) -- TODO: we should be clearing the preview here not rendering it.

    ViewNoteMsg -> (model, Browser.Navigation.load "view.html")

    (NoteSaveResponseMsg noteResponse) ->
      let updatedNote =
            case model.note of
              BrandNewNote            -> model.note -- illegal
              (HavingContent content) -> HavingContent <| noteFromRemoteSave content noteResponse
          updatedModel = {model | remoteSaveStatus = noteResponse, note = updatedNote, noteContentStatus = contentStatusFromRemoteSave noteResponse }
      in (updatedModel, sendSaveMessage updatedModel)

contentStatusFromRemoteSave : RemoteNoteData -> ContentStatus
contentStatusFromRemoteSave remoteData =
  if RemoteData.isSuccess remoteData then UpToDate else NeedsToSave

noteFromRemoteSave : NoteWithContent -> RemoteNoteData -> NoteWithContent
noteFromRemoteSave existingNote remoteData =
  case (existingNote, remoteData) of
    (NoteWithoutId noteText, Success noteId) -> NoteWithId noteId noteText
    (NoteWithoutId _, _)  -> existingNote -- if we didn't succeed in updating the note, then there's no id to save
    ((NoteWithId _ _), _) -> existingNote -- if we already have an id, then there's nothing to update


saveNote: Model -> (Model, Cmd Msg)
saveNote model =
  case model.remoteSaveStatus of
    Loading       -> (model, Cmd.none) -- still loading from a previous save...
    _             ->
      case model.note of
        BrandNewNote -> (model, Cmd.none)
        HavingContent content ->
          let  (newModel, remoteSaveCmd) =  performOrGotoConfig model ({model | remoteSaveStatus = Loading }, performSaveNote content)
          in (newModel, Cmd.batch [remoteSaveCmd, sendSaveMessage newModel]) -- TODO: Change this into a Task

sendSaveMessage: Model -> Cmd Msg
sendSaveMessage model = scribMessage (encode SaveMessage model)


performSaveNote: NoteWithContent ->  ApiKey -> Cmd Msg
performSaveNote note apiKey =
  Http.request {
   method    = "POST"
  , headers  = [apiKeyHeader apiKey]
  , url      = "http://localhost:3000/note"
  , body     = Http.jsonBody <| encodeSaveNote note
  , expect = Http.expectJson processSaveNoteResults D.int
  , timeout  = Nothing
  , tracker  = Nothing
  }

performOrGotoConfig : Model -> (Model, (ApiKey -> Cmd Msg)) -> (Model, Cmd Msg)
performOrGotoConfig oldModel apiKeyCommand =
  performApiKey
    oldModel.apiKey
    apiKeyCommand
    (oldModel, Browser.Navigation.load "config.html")


--performSaveNote: NoteWithContent -> Model -> ApiKey -> (Model, Cmd Msg)
--performSaveNote note model =
--    let remoteCall =
--          Http.post {
--            url = "http://localhost:3000/note" -- This should be configurable
--          , body = Http.jsonBody <| encodeSaveNote note
--          , expect = Http.expectJson processSaveNoteResults D.int
--          }
--    in ({model | remoteSaveStatus = Loading }, remoteCall)

processSaveNoteResults: Result Http.Error Int -> Msg
processSaveNoteResults = processHttpResult NoteSaveResponseMsg


processHttpResult: (RemoteNoteData -> Msg) -> Result Http.Error Int -> Msg
processHttpResult toMsg httpResult   =
  let result  = RemoteData.fromResult httpResult
  in toMsg result

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
      viewNotificationsArea model.remoteSaveStatus
    , viewNotesTextArea model.note
    , viewControls model -- TODO: Fix
    ]


viewNotificationsArea: RemoteNoteData -> Html a
viewNotificationsArea remoteSaveStatus =
  case remoteSaveStatus of
    Failure e   -> addFailureAlert <| "Save failed: " ++ fromHttpError e -- show error
    (Success _) -> addSuccessAlert "Saved note"
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


viewMarkdownPreview : Html Msg
viewMarkdownPreview =
  plainDiv
    [
      hr [] []
    , div [id "markdown-view"] []
    ]

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

encodeSaveNote: NoteWithContent -> E.Value
encodeSaveNote note =
  case note of
    (NoteWithoutId noteText)     -> encodeUnsavedNote noteText
    (NoteWithId noteId noteText) -> encodeSavedNote noteText noteId

encodeSavedNote: String -> Int -> E.Value
encodeSavedNote noteText noteId = N.encodeNote <| N.Note noteText noteId

encodeUnsavedNote: String -> E.Value
encodeUnsavedNote noteText =
  E.object
    [
      ("noteText", E.string noteText)
    ]


encode : PortType -> Model -> E.Value
encode portType model =
  E.object
    [ ("eventType", E.string (showPortType portType))
    , ("noteText", E.string <| getNoteText model.note)
    , ("noteId", maybe E.null E.int <| getNoteId model.note)
    ]

getNoteId : Note -> Maybe Int
getNoteId note =
  case note of
    BrandNewNote           -> Nothing
    (HavingContent (NoteWithId noteId _))  -> Just noteId
    (HavingContent (NoteWithoutId _))      -> Nothing

getNoteText : Note -> String
getNoteText note =
  case note of
    BrandNewNote                             -> ""
    (HavingContent (NoteWithId _ noteText))  -> noteText
    (HavingContent (NoteWithoutId noteText)) -> noteText

--decoderLocalEdits : D.Decoder Model
--decoderLocalEdits =
--  let maybeNoteId   = D.maybe (D.field "noteId" D.int)
--      noteText      =  (D.field "noteText" D.string)
--      remoteSave    = NotAsked
--      noteWithoutId = D.map (HavingContent << NoteWithoutId) noteText
--      noteWithId    = \noteId -> D.map (HavingContent << NoteWithId noteId) noteText
--      note          = D.andThen (maybe noteWithoutId noteWithId) maybeNoteId
--  in D.map (\n -> Model n LocalLoad remoteSave UpToDate) note

-- TODO: What happens if we go directly to Save without saving an edit?
decoderLocalEdits : D.Decoder LocalEdit
decoderLocalEdits =
  let maybeNoteIdDecoder   = D.maybe (D.at ["note", "noteId"] D.int)
      maybeNoteTextDecoder = D.maybe (D.at ["note", "noteText"] D.string)
      apiKeyDecoder        = D.field "apiKey" decodeApiKey
      noteDecoder          =
        D.map2
          maybeNoteContent
          maybeNoteIdDecoder
          maybeNoteTextDecoder
  in D.map2
      LocalEdit
      apiKeyDecoder
      noteDecoder


maybeNoteContent : Maybe Int -> Maybe String -> Note
maybeNoteContent maybeNoteId maybeNoteText =
  case (maybeNoteId, maybeNoteText) of
    (Just noteId, Just noteText) -> HavingContent <| NoteWithId noteId noteText
    (Just noteId, Nothing)       -> BrandNewNote -- technically shouldn't happen, treat it as no note
    (Nothing, Just noteText)     -> HavingContent <| NoteWithoutId noteText
    (Nothing, Nothing)           -> BrandNewNote

--
-- UTIL
--

hasContent: Note -> Bool
hasContent note =
  case note of
    BrandNewNote -> False
    (HavingContent (NoteWithoutId noteText)) -> not (String.isEmpty noteText)
    (HavingContent (NoteWithId _ noteText))  -> not (String.isEmpty noteText)


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
