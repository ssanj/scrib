module Slate exposing (Note(..), encodeNote, decodeNote, getNoteId, getNoteVersion)

import Json.Encode as E
import Json.Decode as D

-- make this opaque so they can't be tampered with.
-- now if we get a  NoteId or NoteVersion we know it came from the server
type NoteId = NoteId Int
type NoteVersion = NoteVersion Int

type Note = NewNote String
          | SavedNote NoteId String NoteVersion

encodeNote: Note -> E.Value
encodeNote note =
  case note of
    (NewNote noteText)                      ->
      E.object
       [
         ("noteText", E.string noteText)
       ]

    (SavedNote (NoteId noteId) noteText (NoteVersion noteVersion)) ->
      E.object
       [
         ("noteId", E.int noteId)
       , ("noteText", E.string noteText)
       , ("noteVersion", E.int noteVersion)
       ]

getNoteId : NoteId -> Int
getNoteId (NoteId noteId) = noteId

getNoteVersion : NoteVersion -> Int
getNoteVersion (NoteVersion noteVersion) = noteVersion

decodeNote: D.Decoder Note
decodeNote =
  D.map3
    SavedNote
    (D.map NoteId <| D.field "noteId" D.int)
    (D.field "noteText" D.string)
    (D.map NoteVersion <| D.field "noteVersion" D.int)

