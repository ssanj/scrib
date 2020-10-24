module Ports exposing
  (
    --encodePort
    --ViewPortType(..)
  --, SavePortType(..)
  --, PortType(..)
    PortTypeName
  , JsCommand(..)
  , JsCommandValue
  , JsStorageValue
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

-- Why do we need a key at all? Just log to the same place each time. Maybe "data" ?
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
  encodePortAndJsPayloadWithStorageAccess
      (PortTypeName <| "save_top_notes_to_session_storage")
      storageValue
      encoder

encodePortAndJsPayloadWithStorageAccess : PortTypeName -> JsStorageValue a -> Encoder a -> E.Value
encodePortAndJsPayloadWithStorageAccess (PortTypeName portType) { storageArea, storageAction, key, value } encoder =
  E.object
    [
      ("eventType", E.string portType)
    , ("storage", encodeStorageAreaAction storageArea storageAction key encoder value)
    ]

--type ViewPortType = PreviewViewNote SCRIB.NoteFull
--                  | SaveViewNoteToLocalStorage SCRIB.NoteFull
--                  | SaveTopNotesToSessionStorage (List SCRIB.NoteFull)
--                  | RemoveNoteFromLocalStorage
--                  | LogMessageToConsole String

--type SavePortType = SaveEditNoteToLocalStorage SCRIB.Note
--                  | PreviewSaveNote SCRIB.Note

--type PortType = ViewPort ViewPortType
--              | SavePort SavePortType

--encodePort : Encoder PortType
--encodePort portType =
--  case portType of
--    ViewPort vp -> encodeViewPortRequest vp
--    SavePort sp -> encodeSavePortRequest sp

--encodeViewPortRequest :  Encoder ViewPortType
--encodeViewPortRequest viewPort =
--  case viewPort of
--    (PreviewViewNote note)                   ->
--      encodePortAndPayload
--        (PortTypeName <| "preview_note")
--        noteKey
--        SCRIB.encodeFullNote
--        note

--    (SaveViewNoteToLocalStorage note)       ->
--      encodePortAndPayloadWithStorageAccess
--        (PortTypeName <| "save_note_to_local_storage")
--        viewSelectedNoteStorageArea
--        noteKey
--        SCRIB.encodeFullNote
--        note

--    (SaveTopNotesToSessionStorage notes) ->
--      encodePortAndPayloadWithStorageAccess
--        (PortTypeName <| "save_top_notes_to_session_storage")
--        viewTopNotesStorageArea
--        topNotesKey
--        SCRIB.encodeFullNotes
--        notes

--    RemoveNoteFromLocalStorage           ->
--      encodePortWithStorageAccess
--        (PortTypeName <| "remove_note_from_local_storage")
--        viewSelectedNoteStorageArea
--    (LogMessageToConsole message)        ->
--      encodePortAndPayload
--        (PortTypeName <| "log_message_to_console")
--        outputKey
--        E.string
--        message

--encodeSavePortRequest : Encoder SavePortType
--encodeSavePortRequest savePort =
--  case savePort of
--    (SaveEditNoteToLocalStorage note) -> encodePortAndPayload (PortTypeName <| "save_note_to_local_storage") noteKey SCRIB.encodeNote note
--    (PreviewSaveNote note)            -> encodePortAndPayload (PortTypeName <| "preview_note") noteKey SCRIB.encodeNote note

--encodePortWithStorageAccess : PortTypeName -> StorageArea ->  E.Value
--encodePortWithStorageAccess (PortTypeName portType) storageArea  =
--  E.object
--    [
--      ("eventType", E.string portType)
--    , ("storage", encodeStorageArea storageArea)
--    ]

--encodePortAndPayloadWithStorageAccess : PortTypeName -> StorageArea -> JsonKey -> Encoder a -> a -> E.Value
--encodePortAndPayloadWithStorageAccess (PortTypeName portType) storageArea key payloadEncoder payload =
--  E.object
--    [
--      ("eventType", E.string portType)
--    , ("storage", encodeStorageArea storageArea)
--    , encodeKeyAndPayload key payloadEncoder payload
--    ]

encodePortAndPayload : PortTypeName -> JsonKey -> Encoder a -> a -> E.Value
encodePortAndPayload (PortTypeName portType) key payloadEncoder payload =
  E.object
    [
      ("eventType", E.string portType)
    , encodeKeyAndPayload key payloadEncoder payload
    ]
