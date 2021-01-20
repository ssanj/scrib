port module Config exposing (..)

import RemoteData      exposing (..)
import Html            exposing (..)
import Html.Attributes exposing (..)
import ElmCommon       exposing (..)
import StorageKeys     exposing (..)
import Notifications   exposing (..)
import TagExtractor    exposing (..)

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


type alias Model = ()


-- MSG


type Msg = ChangeMe


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
init _ = onlyModel ()
    --let decodeResult = D.decodeValue decodeLocalNotes topNotes
    --in handleDecodeResult decodeResult handleInitSuccess handleInitError


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
     ChangeMe                   -> onlyModel model

-- VIEW

view : Model -> Html Msg
view model = div [] [ text "config here"]

--viewFooter : Html msg
--viewFooter =
--  nav
--    [ attribute "aria-label" "main navigation", class "content", attribute "role" "navigation" ]
--    [ div
--        [ class "content has-text-centered" ]
--        [ p
--            [ class "scrib-footer"]
--            [
--              text "scribble effortlessly"
--            ]
--        , div
--          [ class "is-size-7" ]
--          [
--            text "crafted by "
--          , a
--              [ href "https://sanj.ink"]
--              [
--                text "Sanj Sahayam"
--              ]
--          ]
--        ]
--    ]



-- PORTS


port scribMessage : E.Value -> Cmd msg
port jsMessage : (E.Value -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
  --jsMessage (S.encodeJsResponse subscriptionSuccess subscriptionFailure)


-- MODEL HELPERS


-- INIT HELPERS


--handleInitSuccess : ApiKeyWithPayload (List SC.NoteFull) -> (Model, Cmd Msg)
--handleInitSuccess { apiKey, payload } =
--  let notes = maybe [] identity payload
--      mc    =
--        if List.isEmpty notes
--        then
--          let model =
--               { emptyModel |
--                    notes       = Loading
--                  , apiKey      = Just apiKey
--               }
--              (newModel, cmdTimeoutInfoMessage) = handleInlineInfo model <| InformationMessage "No cached data, refreshing"
--              commands =
--                [
--                  getTopRemoteNotes apiKey
--                , cmdTimeoutInfoMessage
--                ]
--          in (newModel, Cmd.batch commands)
--        else onlyModel { emptyModel | retrievedNotes = notes, apiKey = Just apiKey }
-- in mc

-- TODO: Maybe we give the user the option of choosing to go to the config page
-- via an OK button or similar
-- We also want to completely get rid of logMessage
--handleInitError : D.Error -> (Model, Cmd Msg)
--handleInitError err = (
--                        emptyModel
--                      , Cmd.batch
--                        [
--                          Browser.Navigation.load "config.html"
--                        , logMessage ("Decode of init data failed due to: " ++ D.errorToString err)
--                        ]
--                      )

--handleDecodeResult : Result D.Error a -> (a -> b) -> (D.Error -> b) -> b
--handleDecodeResult result success failure =
--  case result of
--    (Ok value)  -> success value
--    (Err error) -> failure error


-- DECODERS


-- UPDATE HELPERS


-- VIEW HELPERS


--viewMenu : Html Msg
--viewMenu =
--  nav [ attribute "aria-label" "main navigation", class "navbar", attribute "role" "navigation" ]
--      [ div [ class "navbar-brand" ]
--          [ a [ attribute "aria-expanded" "false", attribute "aria-label" "menu", class "navbar-burger burger", attribute "data-target" "navbarBasicExample", attribute "role" "button" ]
--              [ span [ attribute "aria-hidden" "true" ]
--                  []
--              , span [ attribute "aria-hidden" "true" ]
--                  []
--              , span [ attribute "aria-hidden" "true" ]
--                  []
--              ]
--          ]
--      , div [ class "navbar-menu", id "navbarBasicExample" ]
--          [ div [ class "navbar-start" ]
--              [ a [ class "navbar-item", href "index.html" ]
--                  [ text "Home" ]
--              , a [ class "navbar-item", href "view.html" ]
--                  [ text "View Notes" ]
--              , a [ class "navbar-item", href "save.html" ]
--                  [ text "New Note" ]
--              ]
--          ]
--      ]


-- JS COMMANDS


--topNotesSavedToSessionStorageResponseKey : P.ResponseKey
--topNotesSavedToSessionStorageResponseKey = P.ResponseKey "TopNotesSavedToSessionStorage"

--appName : String
--appName = "scrib"

--appMessage : a -> P.JsAppMessage a
--appMessage = P.JsAppMessage appName

--logMessage: String -> Cmd Msg
--logMessage message =
--  let logCommand = P.LogConsole <| appMessage message
--  in scribMessage <| P.encodeJsCommand logCommand E.string

--saveSelectedNoteToLocalStorage : SC.NoteFull -> Cmd Msg
--saveSelectedNoteToLocalStorage note =
--  let storageArea             = savedNoteStorageArea
--      saveSelectedNoteValue   = P.JsStorageValue storageArea Save note
--      responseKey             = Just noteSavedToLocalStorageResponseKey
--      saveSelectedNoteCommand = P.WithStorage saveSelectedNoteValue responseKey
--  in scribMessage <| P.encodeJsCommand saveSelectedNoteCommand SC.encodeFullNote

-- SUBSCRIPTION HELPERS

--subscriptionSuccess : S.JsResponse E.Value -> Msg
--subscriptionSuccess (S.JsResponse (P.ResponseKey key) result) =
--  case (key) of
--    "NoteSavedToLocalStorage"       -> NoteSavedToLocalStorage
--    "NoteRemovedFromLocalStorage"   -> NoteRemovedFromLocalStorage
--    "TopNotesSavedToSessionStorage" -> TopNotesSavedToSessionStorage
--    otherKey                      -> subscriptionFailure <| ("Unhandled JS notification: " ++ otherKey)

--subscriptionFailure : String -> Msg
--subscriptionFailure m = JSNotificationError ("subscriptionFailure: " ++ m)
