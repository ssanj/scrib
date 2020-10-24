module Ports exposing
  (
    -- Data types
    PortTypeName
  , JsCommand(..)
  , JsStorageValue

  -- Functions
  , encodeJsCommand
  )

import StorageKeys exposing (..)
import ElmCommon exposing (Encoder)
import Json.Encode as E
import Json.Decode as D
import Note        as SCRIB

type PortTypeName = PortTypeName String

{-| Actions we can perform through JS
  | Preview something
  | Save something to some kind of storage somewhere
  | Delete something to some kind of storage somewhere
  | Log console messages
-}

-- TODO: Why do we need a key at all? Just log to the same place each time. Maybe "data" ?
type alias JsStorageValue a = { storageArea: StorageArea, storageAction: StorageAction, key: JsonKey, value: a }

type JsCommand a = LogConsole a
                 | MarkdownPreview a
                 | WithStorage (JsStorageValue a)


encodeJsCommand : JsCommand a -> Encoder a -> E.Value
encodeJsCommand command encoder =
  case command of
    (LogConsole value)      -> logCommand encoder value
    (MarkdownPreview value) -> markdownPreviewCommand encoder value
    (WithStorage value)     -> withStorageCommand encoder value

logCommand : Encoder a -> a -> E.Value
logCommand = encodePortAndPayload (PortTypeName <| "log_message_to_console")

markdownPreviewCommand : Encoder a -> a -> E.Value
markdownPreviewCommand = encodePortAndPayload (PortTypeName <| "preview_note")

withStorageCommand : Encoder a -> JsStorageValue a -> E.Value
withStorageCommand = encodePortWithStorageAccess (PortTypeName <| "save_top_notes_to_session_storage")

encodePortWithStorageAccess : PortTypeName -> Encoder a -> JsStorageValue a -> E.Value
encodePortWithStorageAccess (PortTypeName portType) encoder { storageArea, storageAction, key, value }  =
  E.object
    [
      ("eventType", E.string portType)
    , ("storage", encodeStorageAreaAction storageArea storageAction key encoder value)
    ]

encodePortAndPayload : PortTypeName -> Encoder a -> a -> E.Value
encodePortAndPayload (PortTypeName portType) payloadEncoder payload =
  E.object
    [
      ("eventType", E.string portType)
    , ("data", payloadEncoder payload)
    ]
