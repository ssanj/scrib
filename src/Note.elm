module Note exposing (..)

import Json.Encode as E
import Json.Decode as D

type alias Note =
  {
    noteText: String
  , noteId: Int
  }

encodeNote: Note -> E.Value
encodeNote {noteText, noteId} =
  E.object
   [
      ("noteText", E.string noteText)
    , ("noteId", E.int noteId)
   ]


decodeNotes : D.Decoder (List Note)
decodeNotes = D.list decodeNote

decodeNote: D.Decoder Note
decodeNote = D.map2 Note (D.field "noteText" D.string) (D.field "noteId" D.int)
