module Ports exposing
  (
    -- Data types
    PortTypeName
  , JsCommand(..)
  , JsStorageValue
  , JsAppMessage

  -- Functions
  , encodeJsCommand
   )

import StorageKeys exposing (..)
import ElmCommon exposing (Encoder)
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

type alias JsStorageValue a = { storageArea: StorageArea, storageAction: StorageAction, value: a }
type alias JsAppMessage a = { appName: String , value: a }

type JsCommand a = LogConsole (JsAppMessage a)
                 | MarkdownPreview a
                 | WithStorage (JsStorageValue a)

encodeJsCommand : JsCommand a -> Encoder a -> E.Value
encodeJsCommand command encoder =
  case command of
    (LogConsole value)      -> logCommand encoder value
    (MarkdownPreview value) -> markdownPreviewCommand encoder value
    (WithStorage value)     -> withStorageCommand encoder value

logCommand : Encoder a -> JsAppMessage a -> E.Value
logCommand = encodePortWithLog logMessagePort

markdownPreviewCommand : Encoder a -> a -> E.Value
markdownPreviewCommand = encodePortAndPayload markdownPreviewPort

withStorageCommand : Encoder a -> JsStorageValue a -> E.Value
withStorageCommand = encodePortWithStorageAccess withStoragePort

encodePortWithStorageAccess : PortTypeName -> Encoder a -> JsStorageValue a -> E.Value
encodePortWithStorageAccess (PortTypeName portType) encoder { storageArea, storageAction, value }  =
  E.object
    [
      ("eventType", E.string portType)
    , ("storage", encodeStorageAreaAction storageArea storageAction encoder value)
    ]

encodePortWithLog : PortTypeName -> Encoder a -> JsAppMessage a -> E.Value
encodePortWithLog (PortTypeName portType) encoder { appName, value }  =
  E.object
    [
      ("eventType", E.string portType)
    , ("data",
        E.object
          [
            (appName, encoder value)
          ]
      )
    ]

encodePortAndPayload : PortTypeName -> Encoder a -> a -> E.Value
encodePortAndPayload (PortTypeName portType) payloadEncoder payload =
  E.object
    [
      ("eventType", E.string portType)
    , ("data", payloadEncoder payload)
    ]
