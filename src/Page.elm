port module Page exposing (..)

import RemoteData      exposing (..)
import Html            exposing (..)
import Html.Attributes exposing (..)
import ElmCommon       exposing (..)
import StorageKeys     exposing (..)
import Notifications   exposing (..)

import String          exposing (toLower)
import Html.Events     exposing (onClick, onInput)
import FP              exposing (maybe, const, collect, maybeToList, find)
import ApiKey          exposing (ApiKey, ApiKeyWithPayload, apiKeyHeader, decodeApiKeyWithPayload, performApiKey)
import Markdown

import Browser.Navigation
import Browser
import Http
import Browser.Navigation


import List.Nonempty    as N
import Json.Decode      as D
import Json.Encode      as E
import Note             as SC
import Ports            as P
import Subs             as S
import Component.Footer as Footer


-- MODEL


type alias Model =
  {
    note : Maybe SC.NoteFull
  }


-- MSG

type Msg = NoteEdited SC.NoteFull
         | NoteForEditingSavedToLocalStorage
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
init noteToView =
    let decodeResult = D.decodeValue decodeLocalSave noteToView
    in handleDecodeResult decodeResult handleInitSuccess handleInitError


decodeLocalSave : D.Decoder (ApiKeyWithPayload SC.NoteFull)
decodeLocalSave = decodeApiKeyWithPayload noteKey SC.decodeFullNote


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    (NoteEdited note)                     -> handleNoteEdited model note
    NoteForEditingSavedToLocalStorage     -> gotoNoteEditScreen model
    (JSNotificationError error)           -> handleJSError model error



-- VIEW

view : Model -> Html Msg
view model =
    plainDiv
      [
        section [class "section"]
          [
            viewMenu
          , createMarkdownPreview model.note
          , maybe emptyDiv viewControls (model.note)
          , Footer.view
          ]
      ]


viewControls : SC.NoteFull -> Html Msg
viewControls fullNote =
    div
    []
    [
      nav
        [
          class "level mt-2"
        ]
        [
          viewLeftButtonGroup fullNote
        ]
    ]


viewLeftButtonGroup: SC.NoteFull -> Html Msg
viewLeftButtonGroup fullNote =
    div
      [
        class "level-left"
      ]
      [
        viewEditButton fullNote
      ]


viewEditButton : SC.NoteFull -> Html Msg
viewEditButton fullNote =
   button
    [
      class "button"
    , class "is-info mt-1"
    , class "level-item"
    , onClick (NoteEdited fullNote)
    ]
    [
      text "Edit"
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
    note = Nothing
  }


-- INIT HELPERS


handleInitSuccess : ApiKeyWithPayload SC.NoteFull -> (Model, Cmd Msg)
handleInitSuccess { payload } = onlyModel { emptyModel | note = payload }


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


---- UPDATE HELPERS


handleNoteEdited : Model -> SC.NoteFull -> (Model, Cmd Msg)
handleNoteEdited model note = (model, saveSelectedNoteForEditingToLocalStorage note)


gotoNoteEditScreen : Model -> (Model, Cmd Msg)
gotoNoteEditScreen model = (model, Browser.Navigation.load "save.html")


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
              [ a [ class "navbar-item", href "index.html" ]
                  [ text "Home" ]
              , a [ class "navbar-item", href "save.html" ]
                  [ text "New Note" ]
              , a [ class "navbar-item", href "view.html"]
                  [ text "View" ]
              , a [ class "navbar-item", href "config.html" ]
                  [ text "Config" ]
              ]
          ]
      ]


createMarkdownPreview: Maybe SC.NoteFull -> Html Msg
createMarkdownPreview = maybe viewMarkdownPreviewDefault viewMarkdownPreview


viewMarkdownPreview : SC.NoteFull -> Html Msg
viewMarkdownPreview fullNote =
  div
    []
    [
      hr
        []
        []
    , div
        [
          id "page-view"
        ]
        [
          div
            [
              id markdownViewId
            , class "clipped"
            ]
            [
              Markdown.toHtml [] (renderMarkdownPreview fullNote)
            ]
         -- , viewControls fullNote
        ]
    ]


renderMarkdownPreview : SC.NoteFull -> String
renderMarkdownPreview fullNote =
  let allLines                = String.lines <| SC.getNoteFullText fullNote
      previewWithCleanHeading = headingWithoutTags allLines
  in String.join "\n" previewWithCleanHeading

viewMarkdownPreviewDefault: Html Msg
viewMarkdownPreviewDefault =
  div []
    [ hr []
      []
    , div [ id "page-view" ]
      [ div [ id markdownViewId ]
        [
          text "No content to display"
        ]
      ]
    ]

markdownViewId : String
markdownViewId = "markdown-page-view"


-- JS COMMANDS


noteSavedForEditingToLocalStorageResponseKey : P.ResponseKey
noteSavedForEditingToLocalStorageResponseKey = P.ResponseKey "NoteForEditingSavedToLocalStorage"

appName : String
appName = "scrib"


appMessage : a -> P.JsAppMessage a
appMessage = P.JsAppMessage appName


logMessage: String -> Cmd Msg
logMessage message =
  let logCommand = P.LogConsole <| appMessage message
  in scribMessage <| P.encodeJsCommand logCommand E.string


saveSelectedNoteForEditingToLocalStorage : SC.NoteFull -> Cmd Msg
saveSelectedNoteForEditingToLocalStorage note =
  let storageArea             = savedNoteStorageArea
      saveSelectedNoteValue   = P.JsStorageValue storageArea Save note
      responseKey             = Just noteSavedForEditingToLocalStorageResponseKey
      saveSelectedNoteCommand = P.WithStorage saveSelectedNoteValue responseKey
  in scribMessage <| P.encodeJsCommand saveSelectedNoteCommand SC.encodeFullNote


-- SUBSCRIPTION HELPERS


subscriptionSuccess : S.JsResponse E.Value -> Msg
subscriptionSuccess (S.JsResponse (P.ResponseKey key) result) =
  case key of
    "NoteForEditingSavedToLocalStorage" -> NoteForEditingSavedToLocalStorage
    otherKey                            -> subscriptionFailure <| ("Unhandled JS notification: " ++ otherKey)


subscriptionFailure : String -> Msg
subscriptionFailure m = JSNotificationError ("subscriptionFailure: " ++ m)
