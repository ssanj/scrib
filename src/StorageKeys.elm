module StorageKeys exposing
  (
      noteKey
    , topNotesKey
    , outputKey
    , apiKeyKey
    , viewTopNotesStorageArea
    , savedNoteStorageArea
    , apiKeyStorageArea
    , encodeKeyAndPayload
    , encodeStorageAreaAction
    , keyValue
    , JsonKey
    , StorageArea
    , StorageType(..)
    , StorageAction(..)
    , StorageValueType(..)
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

type StorageKey       = StorageKey String
type StorageType      = Local | Session
type StorageValueType = ArrayType | HashType
type StorageAction    = Save | Delete | Add StorageValueType | Update StorageValueType

type StorageArea = StorageArea StorageType StorageKey

viewTopNotesStorageArea: StorageArea
viewTopNotesStorageArea = StorageArea Session (StorageKey "scrib.view")

savedNoteStorageArea: StorageArea
savedNoteStorageArea = StorageArea Local (StorageKey "scrib.edit")

apiKeyStorageArea: StorageArea
apiKeyStorageArea = StorageArea Local (StorageKey "scrib.api.key")

encodeStorageType : Encoder StorageType
encodeStorageType st =
  case st of
    Local   -> E.string "local"
    Session -> E.string "session"

encodeStorageAction : Encoder StorageAction
encodeStorageAction storageAction =
  case storageAction of
    Save               -> E.string "save"
    Delete             -> E.string "delete"
    (Add ArrayType)    -> E.string "add_to_array"
    (Add HashType)     -> E.string "add_to_hash"
    (Update ArrayType) -> E.string "update_to_array"
    (Update HashType)  -> E.string "update_to_hash"

encodeStorageAreaAction : StorageArea -> StorageAction -> Encoder a -> a -> E.Value
encodeStorageAreaAction (StorageArea st (StorageKey storageKey)) action payloadEncoder payload =
      E.object
      [
        ("storageType", encodeStorageType st)
      , ("storageKey", E.string storageKey)
      , ("storageAction", encodeStorageAction action)
      , ("data", payloadEncoder payload)
      ]


encodeKeyAndPayload : JsonKey -> Encoder a -> a -> (String, E.Value)
encodeKeyAndPayload (JsonKey key) payloadEncoder payload = (key, payloadEncoder payload)
