module Note exposing (..)

import Json.Encode as E
import Json.Decode as D

type alias Note =
  {
    noteText: String
  , noteId: Int
  , noteVersion: Int
  }

encodeNote: Note -> E.Value
encodeNote {noteText, noteId, noteVersion} =
  E.object
   [
      ("noteText", E.string noteText)
    , ("noteId", E.int noteId)
    , ("noteVersion", E.int noteVersion)
   ]


emptyNotes : List Note
emptyNotes = []

decodeNotes : D.Decoder (List Note)
decodeNotes = D.oneOf [D.list decodeNote, D.null emptyNotes]

decodeNote: D.Decoder Note
decodeNote = D.map3 Note (D.field "noteText" D.string) (D.field "noteId" D.int) (D.field "noteVersion" D.int)
