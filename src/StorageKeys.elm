module StorageKeys exposing
  (
      noteKey
    , topNotesKey
    , outputKey
    , apiKeyKey
    , viewTopNotesStorageArea
    , viewSelectedNoteStorageArea
    , savedNoteStorageArea
    , apiKeyStorageArea
    , encodeKeyAndPayload
    , encodeStorageAreaAction
    , encodeStorageArea
    , keyValue
    , JsonKey
    , StorageArea
    , StorageType(..)
    , StorageAction(..)
  )

import ElmCommon exposing (Encoder)
import Json.Encode as E
import Json.Decode as D


type JsonKey = JsonKey String

apiKeyKey : JsonKey
apiKeyKey = JsonKey "apiKey"

noteKey : JsonKey
noteKey = JsonKey "note"

topNotesKey : JsonKey
topNotesKey = JsonKey "top_notes"

outputKey : JsonKey
outputKey = JsonKey "output"

keyValue : JsonKey -> String
keyValue (JsonKey value) = value

type StorageKey = StorageKey String
type StorageType = Local | Session
type StorageAction = Save | Delete

type StorageArea = StorageArea StorageType StorageKey

viewTopNotesStorageArea: StorageArea
viewTopNotesStorageArea = StorageArea Session (StorageKey "scrib.view")

viewSelectedNoteStorageArea: StorageArea
viewSelectedNoteStorageArea = StorageArea Local (StorageKey "scrib.edit")

savedNoteStorageArea: StorageArea
savedNoteStorageArea = StorageArea Local (StorageKey "scrib.edit")

apiKeyStorageArea: StorageArea
apiKeyStorageArea = StorageArea Local (StorageKey "scrib.api.key")


encodeStorageType : Encoder StorageType
encodeStorageType st =
  case st of
    Local   -> E.string "local"
    Session -> E.string "session"

encodeStorageArea : Encoder StorageArea
encodeStorageArea (StorageArea st (StorageKey key)) =
      E.object
      [
        ("storageType", encodeStorageType st)
      , ("storageKey", E.string key)
      ]

encodeStorageAction : Encoder StorageAction
encodeStorageAction storageAction =
  case storageAction of
    Save   ->  E.string "save"
    Delete ->  E.string "delete"

encodeStorageAreaAction : StorageArea -> StorageAction -> JsonKey -> Encoder a -> a -> E.Value
encodeStorageAreaAction (StorageArea st (StorageKey storageKey)) action payloadKey payloadEncoder payload =
      E.object
      [
        ("storageType", encodeStorageType st)
      , ("storageKey", E.string storageKey)
      , ("storageAction", encodeStorageAction action)
      , encodeKeyAndPayload payloadKey payloadEncoder payload
      ]


encodeKeyAndPayload : JsonKey -> Encoder a -> a -> (String, E.Value)
encodeKeyAndPayload (JsonKey key) payloadEncoder payload = (key, payloadEncoder payload)
