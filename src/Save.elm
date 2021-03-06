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

import Dict             as DICT
import List.Nonempty    as N
import Json.Decode      as D
import Json.Encode      as E
import Note             as SC
import Ports            as P
import Subs             as S
import List.Nonempty    as N
import SlateError       as SL
import Component.Footer as Footer


-- MODEL


-- Where did the note originate?
type DataSource = LocalLoad
                | InitNote
                | UserCreated

type WhatAreWeDoing = SavingNoteRemotely
                    | SavingNoteLocally
                    | UpdatingSessionCache
                    | DeletingNoteRemotely
                    | DeletingNoteFromSessionCache
                    | Idle

type NoteWithContent = NoteWithoutId SC.NoteLight
                     | NoteWithId SC.NoteFull

type ContentStatus = NeedsToSave
                   | UpToDate


type alias SaveModelCommand = ModelCommand Model Msg

type alias RemoteSaveStatus   = Result (HttpError SL.SlateError) (HttpSuccess SC.NoteIdVersion)
type alias RemoteDeleteStatus = Result (HttpError SL.SlateError) (HttpSuccess SC.NoteId)

type SaveType = SaveNewNoteToLocalAfterRemoteSave
              | UpdateNoteToLocalAfterRemoteSave
              | SaveNoteToLocalBeforeRemoteSave


type alias HttpMetaData v =
  {
    url : String
  , statusCode : Int
  , statusText : String
  , statusJson : Result D.Error v
  , headers : DICT.Dict String String
  }

type HttpError e = HttpBadUrl String
                 | HttpTimeout
                 | HttpNetworkError
                 | HttpBadStatus (HttpMetaData e)

type HttpSuccess a = HttpSuccess (HttpMetaData a)


type alias Model =
  {
    note: NoteWithContent
  , dataSource: DataSource
  , remoteSaveStatus : Maybe RemoteSaveStatus
  , noteContentStatus: ContentStatus
  , apiKey: Maybe ApiKey
  , successMessage : Maybe SuccessMessage
  , infoMessage : Maybe InformationMessage
  , errorMessages: Maybe (N.Nonempty ModalError)
  , doing : WhatAreWeDoing
  , showPreview: Bool
  }

type PortType = SaveMessage
              | PreviewMessage


type Msg = NoteSavedMsg
         | NoteEditedMsg String
         | NewNoteMsg
         | DeleteNoteMsg
         | ViewNoteMsg
         | PreviewNoteMsg
         | NoteSaveResponseMsg RemoteSaveStatus
         | NoteDeleteResponseMsg RemoteDeleteStatus
         | NoteSavedToLocalStorage
         | RemoteNoteIdVersionSavedToLocalStorage
         | RemoteNewNoteSavedToToLocalStorage
         | NewNoteSyncedToSessionStorage
         | UpdatedNoteSyncedToSessionStorage
         | NoteDeletedFromSessionStorage
         | JSNotificationError String
         | InlineSuccessMessageTimedOut
         | InlineInfoTimedOut
         | TesterMsg Seconds Msg
         | ErrorModalClosed

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
    DeleteNoteMsg                          -> handleRemoteDelete model
    ViewNoteMsg                            -> handleGoingToView model
    PreviewNoteMsg                         -> handlePreviewNote model
    NoteSavedToLocalStorage                -> handleRemoteSave model
    RemoteNoteIdVersionSavedToLocalStorage -> handleNoteIdVersionSavedToLocalStorage model
    RemoteNewNoteSavedToToLocalStorage     -> handleNewNoteSavedToLocalStorage model
    NewNoteSyncedToSessionStorage          -> handleNewNoteSyncedToSessionStorage model
    UpdatedNoteSyncedToSessionStorage      -> handleUpdatedNoteSyncedToSessionStorage model
    (JSNotificationError error)            -> handleJSError model error
    InlineSuccessMessageTimedOut           -> handleSuccessMessageTimeout model
    InlineInfoTimedOut                     -> handleInfoMessageTimeout model
    TesterMsg timeout realMessage          -> noop model
    ErrorModalClosed                       -> handleErrorModalClose model
    (NoteSaveResponseMsg noteResponse)     -> handleNoteSaveResponse model noteResponse
    (NoteDeleteResponseMsg noteResponse)   -> handleNoteRemotelyDeletedResponse model noteResponse
    NoteDeletedFromSessionStorage          -> handleNoteDeletedFromSessionStorage model

noop : ModelCommand Model Msg
noop = onlyModel

handleErrorModalClose : SaveModelCommand
handleErrorModalClose model = onlyModel { model | errorMessages = Nothing }

--handleInlineTimeout : Model -> (Model, Cmd Msg)
--handleInlineTimeout model = onlyModel <| onInlineTimeout successMessageLens model


--timeoutInlineSuccessMessage : Cmd Msg
--timeoutInlineSuccessMessage = addTimeoutForInlineMessage inlineInfoSuccessTimeout InlineInfoTimedOut

-- VIEW

handleTesterMessage : Model -> Seconds -> Msg -> (Model, Cmd Msg)
handleTesterMessage model timeout realMsg =
  (model, addTimeoutForInlineMessage timeout realMsg)


view : Model -> Html Msg
view model =
  div
    []
    [
      section
        [class "section"]
        [
          viewMenu
        , div [class "container mt-3"]
              [
                viewErrors model.errorMessages
              , viewNoteEditingArea model
              , showMarkDownPreviewIfRequested model.showPreview model.note
              ]
        ]
    , Footer.view
    ]


showMarkDownPreviewIfRequested : Bool -> NoteWithContent -> Html Msg
showMarkDownPreviewIfRequested showPreview note =
  if showPreview then createMarkdownPreview note
  else div [] []


viewErrors : Maybe (N.Nonempty ModalError) -> Html Msg
viewErrors = maybe emptyDiv (\errors -> viewModalErrors errors ErrorModalClosed)


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


performRemoteSaveNote: NoteWithContent -> ApiKey -> Cmd Msg
performRemoteSaveNote note apiKey =
  Http.request {
   method    = "POST"
  , headers  = [ apiKeyHeader apiKey ]
  , url      = "/note"
  , body     = Http.jsonBody <| encodeSaveNote note
  , expect   = Http.expectStringResponse processSaveNoteResults (responseToHttpResponse SL.decodeSlateError SC.decoderNoteIdVersion) -- Http.expectJson processSaveNoteResults SC.decoderNoteIdVersion
  , timeout  = Nothing
  , tracker  = Nothing
  }


performRemoteDeleteNote: SC.NoteFull -> ApiKey -> Cmd Msg
performRemoteDeleteNote noteFull apiKey =
  Http.request {
   method    = "DELETE"
  , headers  = [ apiKeyHeader apiKey ]
  , url      = "/note/" ++ (String.fromInt <| SC.getNoteFullId noteFull)
  , body     = Http.emptyBody
  , expect   = Http.expectStringResponse processDeleteNoteResults (responseToHttpResponse SL.decodeSlateError SC.decoderNoteId)
  , timeout  = Nothing
  , tracker  = Nothing
  }


responseToHttpResponse : D.Decoder e -> D.Decoder a ->  Http.Response String -> Result (HttpError e) (HttpSuccess a)
responseToHttpResponse errorDecoder successDecoder response  =
  case response of
    Http.BadUrl_ url               -> Err <| HttpBadUrl url
    Http.Timeout_                  -> Err HttpTimeout
    Http.NetworkError_             -> Err HttpNetworkError
    Http.BadStatus_ metadata body  -> Err <| HttpBadStatus (toHttpMetaData metadata errorDecoder body)
    Http.GoodStatus_ metadata body -> Ok <| HttpSuccess (toHttpMetaData metadata successDecoder body)


toHttpMetaData : Http.Metadata -> D.Decoder a -> String -> HttpMetaData a
toHttpMetaData meta decoderOfA valueA =
  let maybeContentType      = DICT.get "content-type" meta.headers
      isJsonResponse        = maybe False (String.contains "application/json") maybeContentType
      jsonDecoder           = D.decodeString decoderOfA
      contentType           = maybe "no content type" identity maybeContentType
      errorText             =
          ("Could not decode result because the content type was invalid. Expected: 'application/json', but got: " ++
          contentType ++
          ". Response received: " ++
          valueA)
      invalidContentResult  = Err <| D.Failure "invalid content" <| E.string errorText
      responseJson          = if isJsonResponse then jsonDecoder valueA else invalidContentResult
  in
    {
        url        = meta.url
      , statusCode = meta.statusCode
      , statusText = meta.statusText
      , statusJson = responseJson
      , headers    = meta.headers
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
defaultModel =
  {
    note              = defaultNote
  , dataSource        = InitNote
  , remoteSaveStatus  = Nothing
  , noteContentStatus = UpToDate
  , apiKey            = Nothing
  , successMessage    = Nothing
  , infoMessage       = Nothing
  , errorMessages     = Nothing
  , doing             = Idle
  , showPreview       = False
  }

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

hasBeenSaved : NoteWithContent -> Bool
hasBeenSaved note =
  case note of
    NoteWithoutId _ -> False
    NoteWithId _    -> True


-- TODO: Put the constants in one place
inlineInfoSuccessTimeout : Seconds
inlineInfoSuccessTimeout = Seconds 1


-- REMOTE HELPERS

processSaveNoteResults : RemoteSaveStatus -> Msg
processSaveNoteResults = NoteSaveResponseMsg


processDeleteNoteResults : RemoteDeleteStatus -> Msg
processDeleteNoteResults = NoteDeleteResponseMsg


-- UPDATE HELPERS


handleSavingNote: Model -> (Model, Cmd Msg)
handleSavingNote model =
  let saveNoteToLocalCmd =  saveNoteToLocalStorage model.note SaveNoteToLocalBeforeRemoteSave -- saveEditingNoteToLocalStorage noteSavedToLocalStorageResponseKey model.note
      newModel           =
        {
          model |
            doing            = SavingNoteLocally
          --, remoteNoteData = NotAsked
          , remoteSaveStatus = Nothing
          , infoMessage      = Just <| InformationMessage "Saving Locally"
        }
  in (newModel, saveNoteToLocalCmd)


handleRemoteSave : Model -> (Model, Cmd Msg)
handleRemoteSave model =
  let updatedModel =
        {
          model |
            --remoteNoteData = Loading
            remoteSaveStatus = Nothing
        ,   doing            = SavingNoteRemotely
        ,   infoMessage      = Just <| InformationMessage "Saving Remotely"
        }
  in performOrGotoConfig model (updatedModel, performRemoteSaveNote model.note)


handleRemoteDelete : Model -> (Model, Cmd Msg)
handleRemoteDelete model =
  let updatedModel =
        {
          model |
          doing            = DeletingNoteRemotely
        , infoMessage      = Just <| InformationMessage "Deleting Remotely"
        }
  in
    case model.note of
      NoteWithId fullNote -> performOrGotoConfig model (updatedModel, performRemoteDeleteNote fullNote)
      NoteWithoutId _     -> onlyModel model


handleDeleteNotFromSession : Model -> (Model, Cmd Msg)
handleDeleteNotFromSession model =
  let updatedModel =
        {
          model |
            doing            = DeletingNoteFromSessionCache
        ,   infoMessage      = Just <| InformationMessage "Deleting Note from session cache"
        }
  in
    case model.note of
      NoteWithId fullNote -> (updatedModel, deleteNoteFromSessionCache <| SC.getNoteFullNoteId fullNote)
      NoteWithoutId _     -> onlyModel model


handleJSError : Model -> String -> (Model, Cmd Msg)
handleJSError model error = (model, logMessage error)



handleNoteIdVersionSavedToLocalStorage : Model -> (Model, Cmd Msg)
handleNoteIdVersionSavedToLocalStorage model =
  let newModel =
        {
          model |
            successMessage = Nothing
          , infoMessage    = Just <| InformationMessage "Updated Note Saved Locally"
          , doing          = UpdatingSessionCache
        }
  in (newModel, syncUpdateNoteWithSessionCache model.note)


handleNewNoteSavedToLocalStorage : Model -> (Model, Cmd Msg)
handleNewNoteSavedToLocalStorage model =
  let newModel =
        {
          model |
            successMessage = Nothing
          , infoMessage    = Just <| InformationMessage "New Note Saved Locally"
          , doing          = UpdatingSessionCache
        }
  in (newModel, syncNewNoteWithSessionCache model.note)


handleNoteDeletedFromSessionStorage : Model -> (Model, Cmd Msg)
handleNoteDeletedFromSessionStorage = handleGoingToView
  --let newModel =
  --      {
  --        model |
  --          successMessage = Nothing
  --        , infoMessage    = Just <| InformationMessage "Note Deleted"
  --        , doing          = Idle
  --      }
  --in onlyModel newModel


handleNewNoteSyncedToSessionStorage : ModelCommand Model Msg
handleNewNoteSyncedToSessionStorage model =
  let newModel =
        {
          model |
            successMessage = Nothing
          , infoMessage    = Just <| InformationMessage "New Note Synced to Session"
          , doing          = Idle
        }
  in (newModel, addTimeoutForInlineMessage inlineInfoSuccessTimeout InlineInfoTimedOut)


handleUpdatedNoteSyncedToSessionStorage : ModelCommand Model Msg
handleUpdatedNoteSyncedToSessionStorage model =
  let newModel =
        {
          model |
            successMessage = Nothing
          , infoMessage    = Just <| InformationMessage "Updated Note Synced to Session"
          , doing          = Idle
        }
  in (newModel, addTimeoutForInlineMessage inlineInfoSuccessTimeout InlineInfoTimedOut)


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
    --, remoteNoteData = NotAsked
    , doing            = Idle
  }



handleGoingToView : Model -> (Model, Cmd Msg)
handleGoingToView model = (model, Browser.Navigation.load "view.html")


handlePreviewNote : Model -> (Model, Cmd msg)
handlePreviewNote model = onlyModel { model | showPreview = if model.showPreview then False else True }


handleNoteSaveResponse : Model -> RemoteSaveStatus -> (Model, Cmd Msg)
handleNoteSaveResponse model result =
  case result of
    Err x       ->
        onlyModel
          {
            model |
              remoteSaveStatus  = Nothing
            , doing             = Idle
            , noteContentStatus = NeedsToSave
            , errorMessages     = addErrorMessages (fromRemoteError x SL.showSlateError) model.errorMessages
          }

    (Ok (HttpSuccess meta)) ->
      let responseData = meta.statusJson
      in
      case responseData of
        Err x ->
            let newModel =
                  { model |
                      remoteSaveStatus = Just result
                   ,  errorMessages = addErrorMessage "I could not understand the response from the server :(" model.errorMessages
                   }
                event = logMessage <| "Could not decode json response from server: " ++ (D.errorToString x)
            in (newModel, event)

        Ok noteIdVersion ->
          case model.note of
            NoteWithoutId noteText ->
              let newNote = NoteWithId <| SC.updateNoteIdVersion noteIdVersion noteText
              in saveLocally newNote result SaveNewNoteToLocalAfterRemoteSave model

            NoteWithId fullNote    ->
              if SC.isSameNoteId fullNote noteIdVersion then
                let newNote = NoteWithId <| SC.updateNoteVersion noteIdVersion fullNote
                in saveLocally newNote result UpdateNoteToLocalAfterRemoteSave model
              else onlyModel model -- save completed for some other note, just keep doing what you were doing


handleNoteRemotelyDeletedResponse : Model -> RemoteDeleteStatus -> (Model, Cmd Msg)
handleNoteRemotelyDeletedResponse model result =
  case result of
    Err x       ->
        -- check for already delete and proceed as normal
        let actionResult = fromRemoteErrorSelectively x SL.showSlateError onAlreadyDeletedError
        in
         case actionResult of
          Err err ->
            onlyModel
              {
                model |
                  remoteSaveStatus  = Nothing -- we should probably reset all state that we know of
                , doing             = Idle
                , noteContentStatus = UpToDate
                , errorMessages     = addErrorMessages err model.errorMessages
              }
          Ok _    -> handleDeleteNotFromSession model

    (Ok (HttpSuccess meta)) ->
      let responseData = meta.statusJson
      in
        case responseData of
          Err x ->
              let newModel =
                    { model |
                        doing             = Idle
                      , errorMessages = addErrorMessage "I could not understand the response from the server :(" model.errorMessages
                    }
                  event = logMessage <| "Could not decode json response from server: " ++ (D.errorToString x)
              in (newModel, event)

          Ok _ -> handleDeleteNotFromSession model


onAlreadyDeletedError : SL.SlateError -> Maybe ()
onAlreadyDeletedError { errorId } =
  if errorId == 1006 then Just ()
  else Nothing


fromRemoteError : HttpError e -> (e -> String) -> N.Nonempty String
fromRemoteError error showError =
  case error of
    HttpBadUrl url                 -> N.fromElement <| "The url supplied was invalid: " ++ url
    HttpTimeout                    -> N.fromElement <| "The remote operation timed out"
    HttpNetworkError               -> N.fromElement <| "There was a network error during your remote request"
    HttpBadStatus meta ->
      let heading     = "The following error occurred"
          metaStrings = showMeta meta showError
      in N.cons heading metaStrings


--type alias HttpMetaData v =
--  {
--    url : String
--  , statusCode : Int
--  , statusText : String
--  , statusJson : Result D.Error v
--  , headers : Dict String String
--  }

fromRemoteErrorSelectively : HttpError e -> (e -> String) -> (e -> Maybe b) -> Result (N.Nonempty String) b
fromRemoteErrorSelectively error showError matchError =
  case error of
    HttpBadUrl url                 -> Err <| N.fromElement <| "The url supplied was invalid: " ++ url
    HttpTimeout                    -> Err <| N.fromElement <| "The remote operation timed out"
    HttpNetworkError               -> Err <| N.fromElement <| "There was a network error during your remote request"
    HttpBadStatus meta ->
      case getErrorStatus meta matchError of
        (Just value)  -> Ok value
        Nothing ->
          let heading     = "The following error occurred"
              metaStrings = showMeta meta showError
          in Err <| N.cons heading metaStrings


getErrorStatus :  HttpMetaData a -> (a -> Maybe b) -> Maybe b
getErrorStatus { statusJson } handle =
  case statusJson of
    Err _    -> Nothing
    Ok value -> handle value


showMeta : HttpMetaData a -> (a -> String) -> N.Nonempty String
showMeta  { url, statusCode, statusText, statusJson, headers } showError =
  let urlString = "url: " ++ url
      statusCodeString = "stateCode: " ++ (String.fromInt statusCode)
      statusTextString = "statusText: " ++ statusText
      statusBodyString = "body: " ++
        case statusJson of
          Err x    -> D.errorToString x
          Ok value -> showError value
      headerString = "headers: " ++ (headStrings headers)
  in N.Nonempty urlString [statusCodeString, statusTextString, statusBodyString, headerString]


headStrings : DICT.Dict String String -> String
headStrings dict =
  if DICT.isEmpty dict then "-"
  else
    let keyValuePairs         = DICT.toList dict
        keyValuePairsStrings  = List.map (\(k, v) -> k ++ "=" ++ v) keyValuePairs
        commaSeparatedPairs   = List.intersperse ", " keyValuePairsStrings
        combinedHeaderStrings = List.foldl (\v a -> a ++ v) "" commaSeparatedPairs
    in combinedHeaderStrings


addErrorMessage : String -> Maybe (N.Nonempty ModalError) -> Maybe (N.Nonempty ModalError)
addErrorMessage errorMessage maybeErrorMessages = addErrorMessages (N.fromElement errorMessage) maybeErrorMessages


addErrorMessages : N.Nonempty String -> Maybe (N.Nonempty ModalError) -> Maybe (N.Nonempty ModalError)
addErrorMessages errorMessages maybeErrorMessages =
  case maybeErrorMessages of
    Nothing     -> Just <| N.map createModalError errorMessages
    Just errors -> Just <| N.append (N.map createModalError errorMessages) errors


saveLocally : NoteWithContent -> RemoteSaveStatus -> SaveType -> Model -> (Model, Cmd Msg)
saveLocally newNote remoteSaveStatus saveType model =
  (
    {
      model |
        note              = newNote
      , remoteSaveStatus  = Just remoteSaveStatus
      , doing             = SavingNoteLocally
      , noteContentStatus = UpToDate
      , successMessage    = Just <| SuccessMessage "Saved Note"
    }
  , saveNoteToLocalStorage newNote saveType
  )


handleSuccessMessageTimeout : Model -> (Model, Cmd Msg)
handleSuccessMessageTimeout model = onlyModel { model | successMessage = Nothing }


handleInfoMessageTimeout : Model -> (Model, Cmd Msg)
handleInfoMessageTimeout model = onlyModel { model | infoMessage = Nothing }


-- VIEW HELPERS


viewMenu : Html msg
viewMenu =
  nav [ attribute "aria-label" "main navigation", class "navbar", attribute "role" "navigation" ]
      [ div [ class "navbar-brand" ]
          [ a [ attribute "aria-expanded" "false", attribute "aria-label" "menu", class "navbar-burger burger", attribute "data-target" "navbarBasicExample", attribute "role" "button" ]
              [ span [ attribute "aria-hidden" "true" ]
                  []
              , span [ attribute "aria-hidden" "true" ]
                  []
              , span [ attribute "aria-hidden" "true" ]
                  []
              ]
          ]
      , div [ class "navbar-menu", id "navbarBasicExample" ]
          [ div [ class "navbar-start" ]
              [ a [ class "navbar-item", href "index.html" ]
                  [ text "Home" ]
              , a [ class "navbar-item", href "view.html" ]
                  [ text "View" ]
              , a [ class "navbar-item", href "config.html" ]
                  [ text "Config" ]
              ]
          ]
      ]


viewNoteEditingArea : Model -> Html Msg
viewNoteEditingArea model =
  plainDiv
    [
      viewInlineInfoIfAny model.infoMessage
    , viewInlineSuccessIfAny model.successMessage
    --, viewInlineErrorIfAny model.appErrors
    , viewNotesTextArea model.doing model.note model.noteContentStatus
    , viewControls model.doing model.note model.showPreview
    ]


viewInlineInfoIfAny : Maybe InformationMessage -> Html a
viewInlineInfoIfAny = maybe emptyDiv addInlineInfoFlash


viewInlineSuccessIfAny : Maybe SuccessMessage -> Html a
viewInlineSuccessIfAny = maybe emptyDiv addInlineSuccessFlash


viewNotesTextArea: WhatAreWeDoing -> NoteWithContent -> ContentStatus -> Html Msg
viewNotesTextArea doing note contentStatus =
  case doing of
    Idle  ->
      let modifiedStyle = modifiedTag contentStatus
          textAreaNode  =
            [
              id "note-content"
            , class "textarea"
            , rows 10
            , placeholder "e.g. My awesome idea"
            , onInput NoteEditedMsg, value <| getNoteText note
            ]
      in textarea
            (textAreaNode ++ modifiedStyle)
            []
    _ ->
      textarea [id "note-content", class "textarea", rows 10, disabled  True, value <|  getNoteText note]
        []



viewControls: WhatAreWeDoing -> NoteWithContent -> Bool -> Html Msg
viewControls doing note showPreview =
  div
    []
    [
      nav
        [
          class "level"
        ]
        [
          viewLeftButtonGroup doing note showPreview
        , viewRightButtonGroup doing note
        ]
    ]


viewRightButtonGroup : WhatAreWeDoing -> NoteWithContent -> Html Msg
viewRightButtonGroup doing note =
    div
      [
        class "level-right"
      ]
      [
        viewDeleteNoteButton doing note
      , viewNewNoteButton doing
      ]


viewDeleteNoteButton : WhatAreWeDoing -> NoteWithContent -> Html Msg
viewDeleteNoteButton doing note =
  let showSpinner = isBusy doing
  in
    button
      [
        id "delete-note"
         , onClick DeleteNoteMsg
         , classList
             [
               ("button", True)
             , ("level-item", True)
             , ("is-danger", True)
             , ("mt-1", True)
             , ("is-static", not (hasBeenSaved note) || showSpinner)
             ]
       ]
       [text "Delete"]


viewNewNoteButton : WhatAreWeDoing -> Html Msg
viewNewNoteButton doing =
  let disabled = isBusy doing
  in
    button
      [
        id "new-note"
      , onClick NewNoteMsg
      , classList
          [
            ("button", True)
          , ("level-item", True)
          , ("is-info", True)
          , ("mt-1", True)
          , ("is-static", disabled)
          ]
      ]
      [
        text "New Note"
      ]

modifiedTag : ContentStatus -> List (Attribute msg)
modifiedTag contentStatus =
  case contentStatus of
    UpToDate    -> []
    NeedsToSave -> [
                     style "border-right-width" "1em"
                   , style "border-right-color" "salmon"
                   ]


-- TODO: If we have any errors, we should not show spinner
viewLeftButtonGroup: WhatAreWeDoing -> NoteWithContent -> Bool -> Html Msg
viewLeftButtonGroup doing note showPreview =
    div
      [
        class "level-left"
      ]
      [
        viewSaveButton doing note
      , viewPreviewButton note showPreview
      ]


-- TODO: If we have any errors, we should not show spinner
-- TODO: We should do this patten match once and use it everywhere
viewSaveButton: WhatAreWeDoing -> NoteWithContent -> Html Msg
viewSaveButton doing note =
  let showSpinner = isBusy doing
  in
    button
      [
        id "save-note"
         , onClick NoteSavedMsg
         , classList
             [
               ("button", True)
             , ("level-item", True)
             , ("is-success", True)
             , ("mt-1", True)
             , ("is-static", not (hasContent note) || showSpinner)
             ]
       ]
       [text ("Save" ++ (fromBool showSpinner))]



viewPreviewButton: NoteWithContent -> Bool -> Html Msg
viewPreviewButton note showPreview =
    button
      [
        id "preview-note"
         , onClick PreviewNoteMsg
         , classList
             [
               ("button", True)
             , ("level-item", True)
             , ("is-link", True)
             , ("mt-1", True)
             , ("is-static", not (hasContent note))
             ]
       ]
       [text  <| if showPreview then "Hide Preview" else "Preview"]


fromBool : Bool -> String
fromBool truth = if truth then " ⏱️" else ""

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
  div
    []
    [
      hr
      []
      []
    , div
        [
          id "preview"
        ]
        [
          div
            [
              id markdownViewId
            , class "clipped"
            ]
            [
              Markdown.toHtml [] (renderMarkdownPreview noteText)
            ]
        ]
    ]


renderMarkdownPreview : String -> String
renderMarkdownPreview note =
  let allLines                = String.lines note
      previewWithCleanHeading = headingWithoutTags allLines
  in String.join "\n" previewWithCleanHeading


markdownViewId : String
markdownViewId = "markdown-view"


isBusy : WhatAreWeDoing -> Bool
isBusy doing =
  case doing of
    SavingNoteRemotely           -> True
    SavingNoteLocally            -> True
    UpdatingSessionCache         -> True
    DeletingNoteFromSessionCache -> True
    DeletingNoteRemotely         -> True
    Idle                         -> False


-- JS COMMANDS


noteSavedToLocalStorageResponseKey : P.ResponseKey
noteSavedToLocalStorageResponseKey = P.ResponseKey "NoteSavedToLocalStorage"

remoteNoteIdVersionSavedToLocalStorageResponseKey : P.ResponseKey
remoteNoteIdVersionSavedToLocalStorageResponseKey = P.ResponseKey "RemoteNoteIdVersionSavedToLocalStorage"

remoteNewNoteSavedToToLocalStorageResponseKey : P.ResponseKey
remoteNewNoteSavedToToLocalStorageResponseKey = P.ResponseKey "RemoteNewNoteSavedToToLocalStorage"

remoteNewNoteSyncedToSessionStorageResponseKey : P.ResponseKey
remoteNewNoteSyncedToSessionStorageResponseKey = P.ResponseKey "NewNoteSyncedToSessionStorage"

deleteNoteFromSessionStorageResponseKey : P.ResponseKey
deleteNoteFromSessionStorageResponseKey = P.ResponseKey "DeletedNoteFromSessionStorage"

remoteUpdatedNoteSyncedToSessionStorageResponseKey : P.ResponseKey
remoteUpdatedNoteSyncedToSessionStorageResponseKey = P.ResponseKey "UpdatedNoteSyncedToSessionStorage"

saveNoteToLocalStorage : NoteWithContent -> SaveType -> Cmd Msg
saveNoteToLocalStorage note saveType =
  let storageArea             = savedNoteStorageArea
      saveSelectedNoteValue   = P.JsStorageValue storageArea Save note
      responseKey             =
        case saveType of
          SaveNewNoteToLocalAfterRemoteSave -> remoteNewNoteSavedToToLocalStorageResponseKey
          UpdateNoteToLocalAfterRemoteSave  -> remoteNoteIdVersionSavedToLocalStorageResponseKey
          SaveNoteToLocalBeforeRemoteSave   -> noteSavedToLocalStorageResponseKey
      saveSelectedNoteCommand = P.WithStorage saveSelectedNoteValue (Just responseKey)
  in scribMessage <| P.encodeJsCommand saveSelectedNoteCommand encodeSaveNote


syncNewNoteWithSessionCache : NoteWithContent ->Cmd Msg
syncNewNoteWithSessionCache note =
  let storageArea             = viewTopNotesStorageArea
      saveSelectedNoteValue   = P.JsStorageValue storageArea (Add ArrayType) note
      responseKey             = remoteNewNoteSyncedToSessionStorageResponseKey
      saveSelectedNoteCommand = P.WithStorage saveSelectedNoteValue (Just responseKey)
  in scribMessage <| P.encodeJsCommand saveSelectedNoteCommand encodeSaveNote


deleteNoteFromSessionCache : SC.NoteId -> Cmd Msg
deleteNoteFromSessionCache noteId =
  let storageArea               = viewTopNotesStorageArea
      deleteSelectedNoteValue   = P.JsStorageValue storageArea (Delete ArrayType) noteId
      responseKey               = deleteNoteFromSessionStorageResponseKey
      deleteSelectedNoteCommand = P.WithStorage deleteSelectedNoteValue (Just responseKey)
  in scribMessage <| P.encodeJsCommand deleteSelectedNoteCommand SC.encodeNoteId


-- TODO: This is a duplicate of syncNewNoteWithSessionCache with one param different
syncUpdateNoteWithSessionCache : NoteWithContent ->Cmd Msg
syncUpdateNoteWithSessionCache note =
  let storageArea             = viewTopNotesStorageArea
      saveSelectedNoteValue   = P.JsStorageValue storageArea (Update ArrayType) note
      responseKey             = remoteUpdatedNoteSyncedToSessionStorageResponseKey
      saveSelectedNoteCommand = P.WithStorage saveSelectedNoteValue (Just responseKey)
  in scribMessage <| P.encodeJsCommand saveSelectedNoteCommand encodeSaveNote


-- SUBSCRIPTION HELPERS


subscriptionSuccess : S.JsResponse E.Value -> Msg
subscriptionSuccess (S.JsResponse (P.ResponseKey key) result) =
  case (key) of
    "NoteSavedToLocalStorage"                -> NoteSavedToLocalStorage
    "RemoteNoteIdVersionSavedToLocalStorage" -> RemoteNoteIdVersionSavedToLocalStorage
    "RemoteNewNoteSavedToToLocalStorage"     -> RemoteNewNoteSavedToToLocalStorage
    "NewNoteSyncedToSessionStorage"          -> NewNoteSyncedToSessionStorage
    "UpdatedNoteSyncedToSessionStorage"      -> UpdatedNoteSyncedToSessionStorage
    "DeletedNoteFromSessionStorage"          -> NoteDeletedFromSessionStorage
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