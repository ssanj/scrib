module Note exposing (..)

import ElmCommon exposing (Encoder)
import Json.Encode as E
import Json.Decode as D

type alias NoteFull =
  {
    noteText: String
  , noteId: Int
  , noteVersion: Int
  }

type alias NoteLight =
  {
    noteText: String
  }


type Note = Note NoteFull
          | NoteText NoteLight


encodeNote: Note -> E.Value
encodeNote note =
  case note of
    (Note noteFull)      -> encodeFullNote noteFull
    (NoteText noteLight) -> encodeLightNote noteLight

encodeFullNotes: Encoder (List NoteFull)
encodeFullNotes = E.list encodeFullNote

encodeFullNote: Encoder NoteFull
encodeFullNote {noteText, noteId, noteVersion} =
  E.object
   [
      ("noteText", E.string noteText)
    , ("noteId", E.int noteId)
    , ("noteVersion", E.int noteVersion)
   ]

encodeLightNote: Encoder NoteLight
encodeLightNote {noteText} =
  E.object
   [
      ("noteText", E.string noteText)
   ]

decodeFullNotes: D.Decoder (List NoteFull)
decodeFullNotes = D.list decodeFullNote

decodeFullNote: D.Decoder NoteFull
decodeFullNote =
  D.map3
    NoteFull
    (D.field "noteText" D.string)
    (D.field "noteId" D.int)
    (D.field "noteVersion" D.int)

decodeLightNote: D.Decoder NoteLight
decodeLightNote = D.map NoteLight (D.field "noteText" D.string)
