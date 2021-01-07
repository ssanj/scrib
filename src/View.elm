port module View exposing (..)

import RemoteData      exposing (..)
import Html            exposing (..)
import Html.Attributes exposing (..)
import ElmCommon       exposing (..)
import StorageKeys     exposing (..)
import Notifications   exposing (..)

import Html.Events     exposing (onClick, onInput)
import FP              exposing (maybe, const, collect, maybeToList, find)
import ApiKey          exposing (ApiKey, ApiKeyWithPayload, apiKeyHeader, decodeApiKeyWithPayload, performApiKey)
import Markdown

import Browser.Navigation
import Browser
import Http
import Browser.Navigation

import List.Nonempty as N
import Json.Decode   as D
import Json.Encode   as E
import Note          as SC
import Ports         as P
import Subs          as S

-- MODEL

type alias NoteSelection = { retrievedNotes : List SC.NoteFull,  searchResultNotes : List SC.NoteFull, whichNotes : NotesDataSource}

type NotesDataSource = TopNotes
                     | SearchResultNotes

type alias Model =
  {
    query             : Maybe String
  , notes             : RemoteNotesData
  , selectedNote      : Maybe SC.NoteFull
  , apiKey            : Maybe ApiKey
  , appErrors         : Maybe AppErrors
  , retrievedNotes    : List SC.NoteFull
  , searchResultNotes : List SC.NoteFull
  , infoMessage       : Maybe InformationMessage
  , whichNotes        : NotesDataSource
  }

type SaveType = SaveResponse | DontSaveResponse

type alias RemoteNotesData = WebData (List SC.NoteFull)


-- MSG


type Msg = NoteSelected SC.NoteFull
         | NoteEdited SC.NoteFull
         | TopNotesSavedToSessionStorage
         | NoteSavedToLocalStorage
         | NoteRemovedFromLocalStorage
         | JSNotificationError String
         | AddNote
         | SearchEdited String
         | NotesRefreshed
         | TopNotesResponse RemoteNotesData
         | SearchNotesResponse RemoteNotesData
         | ErrorModalClosed
         | InlineErrorTimedOut
         | InlineInfoTimedOut



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
init topNotes =
    let decodeResult = D.decodeValue decodeLocalNotes topNotes
    in handleDecodeResult decodeResult handleInitSuccess handleInitError


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    (NoteSelected note)                   -> handleNoteSelected model note
    (NoteEdited note)                     -> handleNoteEdited model note
    TopNotesSavedToSessionStorage         -> handleTopNotesSavedToSessionStorage model
    NoteSavedToLocalStorage               -> gotoConfigScreen model
    NoteRemovedFromLocalStorage           -> gotoConfigScreen model
    (JSNotificationError error)           -> handleJSError model error
    AddNote                               -> handleAddNote model
    (TopNotesResponse slateCallResult)    -> handleTopNotesResponse model slateCallResult
    (SearchNotesResponse slateCallResult) -> handleSearchResponse model slateCallResult
    NotesRefreshed                        -> handleNotesRefreshed model
    (SearchEdited query)                  -> handleSearchQuery model query
    ErrorModalClosed                      -> handleErrorModalClosed model
    InlineErrorTimedOut                   -> handleInlineErrorTimeout model
    InlineInfoTimedOut                    -> handleInlineInfoTimeout model


-- VIEW

view : Model -> Html Msg
view model =
  let (maybeInlineErrors, maybeModalErrors) = maybe (Nothing, Nothing) getErrors model.appErrors
      notesList                             = choseWhichNotes (noteSelection model)
  in
    plainDiv
      [
        section [class "section"]
          ([ viewMenu ]                                                              ++
          (viewSearchArea notesList model.query maybeInlineErrors model.infoMessage) ++
          [
            viewModalErrorsIfAny maybeModalErrors ErrorModalClosed
          , createMarkdownPreview model.selectedNote
          ])
      , viewFooter
      ]

viewFooter : Html msg
viewFooter =
  nav
    [ attribute "aria-label" "main navigation", class "content", attribute "role" "navigation" ]
    [ div
        [ class "content has-text-centered" ]
        [ p
            [ class "scrib-footer"]
            [
              text "scribble effortlessly"
            ]
        , div
          [ class "is-size-7" ]
          [
            text "crafted by "
          , a
              [ href "https://sanj.ink"]
              [
                text "Sanj Sahayam"
              ]
          ]
        ]
    ]

viewSearchArea : List SC.NoteFull ->  Maybe String -> Maybe InlineError -> Maybe InformationMessage -> List (Html Msg)
viewSearchArea notesList maybeQuery  maybeInlineErrors maybeInfoMessage =
    [
      viewSearchBar maybeQuery
    , viewInlineErrorsIfAny maybeInlineErrors
    , viewInformationIfAny maybeInfoMessage
    , viewNotesList notesList
    ]


viewSearchBar : Maybe String -> Html Msg
viewSearchBar maybeQuery  =
  div [ class "panel-block" ]
    [ p [ class "control has-icons-left" ]
      [ input [ class "input", class "is-primary", placeholder "Search", type_ "text", onInput SearchEdited, value <| getQueryText maybeQuery ]
        []
      , span [ class "icon is-left" ]
        [ i [ attribute "aria-hidden" "true", class "fas", class "fa-search" ]
          []
        ]
      ]
    ]


-- PORTS


port scribMessage : E.Value -> Cmd msg
port jsMessage : (E.Value -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = jsMessage (S.encodeJsResponse subscriptionSuccess subscriptionFailure)


-- MODEL HELPERS


emptyModel: Model
emptyModel =
  {
    query             = Nothing
  , notes             = NotAsked
  , selectedNote      = Nothing
  , apiKey            = Nothing
  , appErrors         = Nothing
  , retrievedNotes    = []
  , searchResultNotes = []
  , infoMessage       = Nothing
  , whichNotes        = TopNotes
  }

inlineInfoTimeout : Seconds
inlineInfoTimeout = Seconds 5

inlineInfoSuccessTimeout : Seconds
inlineInfoSuccessTimeout = Seconds 1

inlineErrorTimeout : Seconds
inlineErrorTimeout = Seconds 3

appErrorsGetter : Model -> Maybe AppErrors
appErrorsGetter model = model.appErrors

appErrorsSetter : Maybe AppErrors -> Model -> Model
appErrorsSetter appErrors model = { model | appErrors = appErrors }

informationMessageGetter : Model -> Maybe InformationMessage
informationMessageGetter model = model.infoMessage

informationMessageSetter : Maybe InformationMessage -> Model -> Model
informationMessageSetter infoMessage model = { model | infoMessage = infoMessage }

handleSearchQuery : Model -> String -> (Model, Cmd Msg)
handleSearchQuery model query =
  let trimmedQuery = String.trim query
  in
    case trimmedQuery of
      ""            -> onlyModel { model | query = Nothing, whichNotes = TopNotes }
      nonEmptyQuery -> performOrGotoConfig model ({ model | query = Just nonEmptyQuery }, searchRemoteNotes nonEmptyQuery)

noteSelection  : Model -> NoteSelection
noteSelection { retrievedNotes,  searchResultNotes, whichNotes } =
  {
    retrievedNotes    = retrievedNotes
  , searchResultNotes = searchResultNotes
  , whichNotes        = whichNotes
  }

choseWhichNotes : NoteSelection -> List SC.NoteFull
choseWhichNotes { retrievedNotes,  searchResultNotes, whichNotes } =
  case whichNotes of
    TopNotes          -> retrievedNotes
    SearchResultNotes -> searchResultNotes


-- INIT HELPERS


handleInitSuccess : ApiKeyWithPayload (List SC.NoteFull) -> (Model, Cmd Msg)
handleInitSuccess { apiKey, payload } =
  let notes = maybe [] identity payload
      mc    =
        if List.isEmpty notes
        then
          let model =
               { emptyModel |
                    notes       = Loading
                  , apiKey      = Just apiKey
               }
              (newModel, cmdTimeoutInfoMessage) = handleInlineInfo model <| InformationMessage "No cached data, refreshing"
              commands =
                [
                  getTopRemoteNotes apiKey
                , cmdTimeoutInfoMessage
                ]
          in (newModel, Cmd.batch commands)
        else onlyModel { emptyModel | retrievedNotes = notes, apiKey = Just apiKey }
 in mc

-- TODO: Maybe we give the user the option of choosing to go to the config page
-- via an OK button or similar
-- We also want to completely get rid of logMessage
handleInitError : D.Error -> (Model, Cmd Msg)
handleInitError err = (
                        emptyModel
                      , Cmd.batch
                        [
                          Browser.Navigation.load "config.html"
                        , logMessage ("Decode of init data failed due to: " ++ D.errorToString err)
                        ]
                      )

handleDecodeResult : Result D.Error a -> (a -> b) -> (D.Error -> b) -> b
handleDecodeResult result success failure =
  case result of
    (Ok value)  -> success value
    (Err error) -> failure error


-- DECODERS


decodeLocalNotes : D.Decoder (ApiKeyWithPayload (List SC.NoteFull))
decodeLocalNotes = decodeApiKeyWithPayload topNotesKey SC.decodeFullNotes


-- REMOTE API CALLS


getTopRemoteNotes: ApiKey -> Cmd Msg
getTopRemoteNotes apiKey =
  Http.request {
   method    = "GET"
  , headers  = [apiKeyHeader apiKey]
  , url      = "/notes"
  , body     = Http.emptyBody
  , expect   = Http.expectJson (RemoteData.fromResult >> TopNotesResponse) SC.decodeFullNotes
  , timeout  = Nothing
  , tracker  = Nothing
  }

searchRemoteNotes: String -> ApiKey -> Cmd Msg
searchRemoteNotes query apiKey =
  Http.request {
   method    = "GET"
  , headers  = [apiKeyHeader apiKey]
  , url      = "/search?q=" ++ query
  , body     = Http.emptyBody
  , expect   = Http.expectJson (RemoteData.fromResult >> SearchNotesResponse) SC.decodeFullNotes
  , timeout  = Nothing
  , tracker  = Nothing
  }

performOrGotoConfig : Model -> (Model, (ApiKey -> Cmd Msg)) -> (Model, Cmd Msg)
performOrGotoConfig oldModel apiKeyCommand =
    performApiKey
    oldModel.apiKey
    apiKeyCommand
    (oldModel, Browser.Navigation.load "config.html")


-- UPDATE HELPERS

handleTopNotesResponse: Model -> RemoteNotesData -> (Model, Cmd Msg)
handleTopNotesResponse model remoteData =
  case remoteData of
    (Failure e)          -> onlyModel <| addModalError appErrorsGetter appErrorsSetter model (ErrorMessage <| fromHttpError e)
    (Success notes) as r ->
      let newModel = { model | retrievedNotes = notes, notes = r, whichNotes = TopNotes }
      in (newModel, Cmd.batch [handleInlineInfoSuccess, saveTopNotesToSessionStorage notes])
    NotAsked             -> handleInlineInfo model <| InformationMessage "No Data"
    Loading              -> handleInlineInfo model <| InformationMessage "Loading..."

handleSearchResponse: Model -> RemoteNotesData -> (Model, Cmd Msg)
handleSearchResponse model remoteData =
  case remoteData of
    (Failure e)          -> addInlineError appErrorsGetter appErrorsSetter model (ErrorMessage <| fromHttpError e) inlineErrorTimeout InlineErrorTimedOut
    (Success notes) as r ->
      let newModel = { model | searchResultNotes = notes, notes = r, whichNotes = SearchResultNotes }
      in (newModel, handleInlineInfoSuccess)
    NotAsked             -> handleInlineInfo model <| InformationMessage "No Data"
    Loading              -> handleInlineInfo model <| InformationMessage "Loading..."

handleInlineInfo : Model -> InformationMessage -> (Model, Cmd Msg)
handleInlineInfo model message =
  addInlineInfo informationMessageSetter model message inlineInfoTimeout InlineInfoTimedOut

-- If the event that the info message that you put up for (eg. Loading...) has completed
-- expedite completion to the next second. Adding a one second delay ensures the messages
-- don't jitter if they complete too quickly.
handleInlineInfoSuccess : Cmd Msg
handleInlineInfoSuccess = addTimeoutForInlineMessage inlineInfoSuccessTimeout InlineInfoTimedOut

handleInlineInfoTimeout : Model -> (Model, Cmd Msg)
handleInlineInfoTimeout model = onlyModel <| onInlineInfoTimeout informationMessageGetter informationMessageSetter model

handleInlineErrorTimeout : Model -> (Model, Cmd Msg)
handleInlineErrorTimeout model = onlyModel <| onInlineErrorTimeout appErrorsGetter appErrorsSetter model

handleErrorModalClosed : Model -> (Model, Cmd Msg)
handleErrorModalClosed model = onlyModel <| onErrorModalClosed appErrorsGetter appErrorsSetter model

handleTopNotesSavedToSessionStorage : Model -> (Model, Cmd Msg)
handleTopNotesSavedToSessionStorage model = onlyModel {model | selectedNote = Nothing }

handleAddNote : Model -> (Model, Cmd Msg)
handleAddNote model = (model, removeSelectedNoteFromLocalStorage)

handleNoteEdited : Model -> SC.NoteFull -> (Model, Cmd Msg)
handleNoteEdited model note = (model, saveSelectedNoteToLocalStorage note)

handleNoteSelected : Model -> SC.NoteFull -> (Model, Cmd Msg)
handleNoteSelected model note = onlyModel  { model| selectedNote = Just note }

gotoConfigScreen : Model -> (Model, Cmd Msg)
gotoConfigScreen model = (model, Browser.Navigation.load "save.html")

handleNotesRefreshed : Model -> (Model, Cmd Msg)
handleNotesRefreshed model = performOrGotoConfig model ({ model | notes = Loading, query = Nothing }, getTopRemoteNotes)

handleJSError : Model -> String -> (Model, Cmd Msg)
handleJSError model error = (model, logMessage error)


-- VIEW HELPERS


viewMenu : Html Msg
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
              [ a [ class "navbar-item", href "/" ]
                  [ text "Home" ]
              , a [ class "navbar-item", href "save.html" ]
                  [ text "New Note" ]
              , a [ class "navbar-item", onClick NotesRefreshed]
                  [ text "Refresh" ]
              , a [ class "navbar-item", href "config.html" ]
                  [ text "Config" ]
              ]
          ]
      ]

getQueryText : Maybe String -> String
getQueryText = maybe "" identity

getNoteCount: List SC.NoteFull -> String
getNoteCount = listFold "-" (String.fromInt << N.length)

listFold: b -> (N.Nonempty a -> b) -> List a -> b
listFold onEmpty onFull elements =
  maybe onEmpty onFull <| N.fromList elements

viewNotesList: List SC.NoteFull -> Html Msg
viewNotesList notes = div [ id "notes-list" ] (List.map createNoteItem notes)

getInformationFromRemoteNotesData : RemoteNotesData -> Maybe InformationMessage
getInformationFromRemoteNotesData remoteNotesData =
  let maybeInfoContent =
        case remoteNotesData of
          NotAsked    -> Just <| InformationMessage "No Data"
          Loading     -> Just <| InformationMessage "Loading..."
          (Failure _) -> Nothing
          (Success _) -> Nothing
  in maybeInfoContent

viewRemoteCallStatus: RemoteNotesData -> Html Msg
viewRemoteCallStatus remoteNotesData =
  let infoContent =
        case remoteNotesData of
          NotAsked      -> [div [] [text "No Data"]]
          Loading       -> [div [] [text "Loading..."]]
          Failure e     -> []
          Success notes -> []
  in div [ id "remote-call-status" ] infoContent

fromHttpError: Http.Error -> String
fromHttpError error =
  case error of
    (Http.BadUrl burl)      -> "bad url: " ++ burl
    Http.Timeout            -> "timeout"
    Http.NetworkError       -> "network error"
    (Http.BadStatus status) -> "bad status: " ++ String.fromInt status
    (Http.BadBody body)     -> "bad body: " ++ body


createMarkdownPreview: Maybe SC.NoteFull -> Html Msg
createMarkdownPreview = maybe viewMarkdownPreviewDefault viewMarkdownPreview

createNoteItem: SC.NoteFull -> Html Msg
createNoteItem fullNote =
  a [class "panel-block", onClick (NoteSelected fullNote)]
  [ span [class "panel-icon"]
    [ i [ class "fas", class "fa-book", attribute "aria-hidden" "true"]
      []
    ]
  , text <| removeHeading <| onlyHeading (SC.getNoteFullText fullNote)
   ]

onlyHeading : String -> String
onlyHeading note = maybe "-no title-" identity (List.head <| String.split "\n" note)

removeHeading : String -> String
removeHeading = String.replace "# " ""

viewMarkdownPreview : SC.NoteFull -> Html Msg
viewMarkdownPreview fullNote =
  div []
    [ hr []
      []
    , div [ id "preview" ]
      [ div [ id markdownViewId ]
        [Markdown.toHtml [] (SC.getNoteFullText fullNote)]
      , button [ class "button", class "is-info", onClick (NoteEdited fullNote) ]
        [ text "Edit" ]
      ]
    ]

viewMarkdownPreviewDefault: Html Msg
viewMarkdownPreviewDefault =
  div []
    [ hr []
      []
    , div [ id "preview" ]
      [ div [ id markdownViewId ]
        []
      ]
    ]

markdownViewId : String
markdownViewId = "markdown-view"

viewModalErrorsIfAny : Maybe ModalErrors -> a -> Html a
viewModalErrorsIfAny maybeError msg = maybe emptyDiv (\errors -> viewModalErrors errors msg) maybeError

viewInlineErrorsIfAny : Maybe InlineError -> Html a
viewInlineErrorsIfAny = maybe emptyDiv viewInlineError

viewInformationIfAny : Maybe InformationMessage -> Html a
viewInformationIfAny = maybe emptyDiv addInlineInfoFlash


-- JS COMMANDS


topNotesSavedToSessionStorageResponseKey : P.ResponseKey
topNotesSavedToSessionStorageResponseKey = P.ResponseKey "TopNotesSavedToSessionStorage"

noteSavedToLocalStorageResponseKey : P.ResponseKey
noteSavedToLocalStorageResponseKey = P.ResponseKey "NoteSavedToLocalStorage"

noteRemovedFromLocalStorageResponseKey : P.ResponseKey
noteRemovedFromLocalStorageResponseKey = P.ResponseKey "NoteRemovedFromLocalStorage"

appName : String
appName = "scrib"

appMessage : a -> P.JsAppMessage a
appMessage = P.JsAppMessage appName

logMessage: String -> Cmd Msg
logMessage message =
  let logCommand = P.LogConsole <| appMessage message
  in scribMessage <| P.encodeJsCommand logCommand E.string

saveSelectedNoteToLocalStorage : SC.NoteFull -> Cmd Msg
saveSelectedNoteToLocalStorage note =
  let storageArea             = savedNoteStorageArea
      saveSelectedNoteValue   = P.JsStorageValue storageArea Save note
      responseKey             = Just noteSavedToLocalStorageResponseKey
      saveSelectedNoteCommand = P.WithStorage saveSelectedNoteValue responseKey
  in scribMessage <| P.encodeJsCommand saveSelectedNoteCommand SC.encodeFullNote

removeSelectedNoteFromLocalStorage : Cmd Msg
removeSelectedNoteFromLocalStorage =
  let storageArea               = savedNoteStorageArea
      removeSelectedNoteValue   = P.JsStorageValue storageArea Delete ()
      responseKey               = Just noteRemovedFromLocalStorageResponseKey
      removeSelectedNoteCommand = P.WithStorage removeSelectedNoteValue responseKey
  in scribMessage <| P.encodeJsCommand removeSelectedNoteCommand (const E.null)

saveTopNotesToSessionStorage : List SC.NoteFull -> Cmd Msg
saveTopNotesToSessionStorage notes =
  let storageArea         = viewTopNotesStorageArea
      saveTopNotesValue   = P.JsStorageValue storageArea Save notes
      responseKey         = Just topNotesSavedToSessionStorageResponseKey
      saveTopNotesCommand = P.WithStorage saveTopNotesValue responseKey
  in scribMessage <| P.encodeJsCommand saveTopNotesCommand SC.encodeFullNotes


-- SUBSCRIPTION HELPERS

subscriptionSuccess : S.JsResponse E.Value -> Msg
subscriptionSuccess (S.JsResponse (P.ResponseKey key) result) =
  case (key) of
    "NoteSavedToLocalStorage"       -> NoteSavedToLocalStorage
    "NoteRemovedFromLocalStorage"   -> NoteRemovedFromLocalStorage
    "TopNotesSavedToSessionStorage" -> TopNotesSavedToSessionStorage
    otherKey                      -> subscriptionFailure <| ("Unhandled JS notification: " ++ otherKey)

subscriptionFailure : String -> Msg
subscriptionFailure m = JSNotificationError ("subscriptionFailure: " ++ m)
