module Ports exposing
  (
    -- Data types
    PortTypeName
  , JsCommand(..)
  , JsCommandValue
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
type alias JsCommandValue a = { key: JsonKey, value: a }
type alias JsStorageValue a = { storageArea: StorageArea, storageAction: StorageAction, key: JsonKey, value: a }

type JsCommand a = LogConsole (JsCommandValue a)
                 | MarkdownPreview (JsCommandValue a)
                 | ToStorage (JsStorageValue a)


encodeJsCommand : JsCommand a -> Encoder a -> E.Value
encodeJsCommand command encoder =
  case command of
    (LogConsole value)      -> logCommand encoder value
    (MarkdownPreview value) -> markdownPreviewCommand encoder value
    (ToStorage value)   -> toStorageCommand encoder value

logCommand : Encoder a -> JsCommandValue a -> E.Value
logCommand encoder { key, value } =
  encodePortAndPayload
    (PortTypeName <| "log_message_to_console")
    key
    encoder
    value

markdownPreviewCommand : Encoder a -> JsCommandValue a -> E.Value
markdownPreviewCommand encoder { key, value } =
  encodePortAndPayload
    (PortTypeName <| "preview_note")
    key
    encoder
    value

toStorageCommand : Encoder a -> JsStorageValue a -> E.Value
toStorageCommand encoder storageValue =
  encodePortWithStorageAccess
      (PortTypeName <| "save_top_notes_to_session_storage")
      storageValue
      encoder

encodePortWithStorageAccess : PortTypeName -> JsStorageValue a -> Encoder a -> E.Value
encodePortWithStorageAccess (PortTypeName portType) { storageArea, storageAction, key, value } encoder =
  E.object
    [
      ("eventType", E.string portType)
    , ("storage", encodeStorageAreaAction storageArea storageAction key encoder value)
    ]

encodePortAndPayload : PortTypeName -> JsonKey -> Encoder a -> a -> E.Value
encodePortAndPayload (PortTypeName portType) key payloadEncoder payload =
  E.object
    [
      ("eventType", E.string portType)
    , encodeKeyAndPayload key payloadEncoder payload
    ]
