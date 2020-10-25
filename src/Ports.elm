module Ports exposing
  (
    -- Data types
    PortTypeName
  , JsCommand(..)
  , ResponseKey(..)
  , JsStorageValue
  , JsAppMessage
  , JsMarkdownValue

  -- Functions
  , encodeJsCommand
   )

import StorageKeys exposing (..)
import ElmCommon exposing (Encoder)
import FP        exposing (maybe)
import Json.Encode as E
import Json.Decode as D
import Note        as SCRIB

type PortTypeName = PortTypeName String

logMessagePort : PortTypeName
logMessagePort = PortTypeName "log_action"

markdownPreviewPort : PortTypeName
markdownPreviewPort = PortTypeName "markdown_action"

withStoragePort : PortTypeName
withStoragePort = PortTypeName "storage_action"

{-| Actions we can perform through JS
  | Preview something
  | Save something to some kind of storage somewhere
  | Delete something to some kind of storage somewhere
  | Log console messages
-}

type ResponseKey = ResponseKey String

type alias JsStorageValue a = { storageArea: StorageArea, storageAction: StorageAction, value: a }
type alias JsAppMessage a = { appName: String , value: a }
type alias JsMarkdownValue a = { elementId: String, value: a }

type JsCommand a = LogConsole (JsAppMessage a)
                 | MarkdownPreview (JsMarkdownValue a)
                 | WithStorage (JsStorageValue a) (Maybe ResponseKey)

encodeJsCommand : JsCommand a -> Encoder a -> E.Value
encodeJsCommand command encoder =
  case command of
    (LogConsole value)      -> logCommand encoder value
    (MarkdownPreview value) -> markdownPreviewCommand encoder value
    (WithStorage value key) -> withStorageCommand encoder value key

logCommand : Encoder a -> JsAppMessage a -> E.Value
logCommand = encodePortWithLog logMessagePort

markdownPreviewCommand : Encoder a -> JsMarkdownValue a -> E.Value
markdownPreviewCommand = encodePortWithMarkdownCommand markdownPreviewPort

withStorageCommand : Encoder a -> JsStorageValue a -> (Maybe ResponseKey) -> E.Value
withStorageCommand = encodePortWithStorageAccess withStoragePort

encodePortWithStorageAccess : PortTypeName -> Encoder a -> JsStorageValue a -> (Maybe ResponseKey) -> E.Value
encodePortWithStorageAccess portType encoder { storageArea, storageAction, value } maybeKey =
  E.object
    [
      encodeEventTypeTuple portType
    , ("storage", encodeStorageAreaAction storageArea storageAction encoder value)
    , ("responseKey", maybe E.null (\(ResponseKey key) -> E.string key) maybeKey)
    ]

encodePortWithLog : PortTypeName -> Encoder a -> JsAppMessage a -> E.Value
encodePortWithLog portType encoder { appName, value }  =
  E.object
    [
      encodeEventTypeTuple portType
    , ("data", -- should this be "log" and { "appName" : "..", "data", "message"} for consistency?
        E.object
          [
            (appName, encoder value)
          ]
      )
    ]

encodePortWithMarkdownCommand : PortTypeName -> Encoder a -> JsMarkdownValue a -> E.Value
encodePortWithMarkdownCommand portType encoder { elementId, value } =
  E.object
    [
      encodeEventTypeTuple portType
    , ("markdown",
        E.object
          [
            ("elementId", E.string elementId)
          , encodeDataTuple encoder value
          ]
      )
    ]

encodeEventTypeTuple : PortTypeName -> (String, E.Value)
encodeEventTypeTuple (PortTypeName portType) = ("eventType", E.string portType)

encodeDataTuple : Encoder a -> a -> (String, E.Value)
encodeDataTuple encoder value = ("data", encoder value)

encodePortAndPayload : PortTypeName -> Encoder a -> a -> E.Value
encodePortAndPayload portType payloadEncoder payload =
  E.object
    [
      encodeEventTypeTuple portType
    , encodeDataTuple payloadEncoder payload
    ]
