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

type alias NoteIdVersion = { noteId : Int, noteVersion : Int }

-- TODO: Make the types in this class opaque

type Note = Note NoteFull
          | NoteText NoteLight

getNoteLightText : NoteLight -> String
getNoteLightText { noteText } = noteText

getNoteFullText : NoteFull -> String
getNoteFullText { noteText } = noteText

getNoteText : Note -> String
getNoteText note =
  case note of
    (Note { noteText })     -> noteText
    (NoteText { noteText }) -> noteText

getNoteId : Note -> Maybe Int
getNoteId note =
  case note of
    (Note { noteId }) -> Just noteId
    _                 -> Nothing

getNoteVersion : Note -> Maybe Int
getNoteVersion note =
  case note of
    (Note { noteVersion }) -> Just noteVersion
    _                      -> Nothing

getNoteIdNoteFull : NoteFull -> Int
getNoteIdNoteFull { noteId } = noteId

getNoteVersionNoteFull : NoteFull -> Int
getNoteVersionNoteFull { noteVersion } = noteVersion


updateNoteText : String -> NoteFull -> NoteFull
updateNoteText newText { noteId, noteVersion } = { noteId = noteId, noteVersion = noteVersion, noteText = newText }

updateNoteIdVersion : NoteIdVersion -> NoteLight -> NoteFull
updateNoteIdVersion { noteId, noteVersion } { noteText } = { noteId = noteId, noteVersion = noteVersion, noteText = noteText }

updateNoteVersion : NoteIdVersion -> NoteFull -> NoteFull
updateNoteVersion { noteVersion } noteFull =  { noteFull | noteVersion = noteVersion }

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

decodeNote : D.Decoder Note
decodeNote =
  D.oneOf
    [
      D.map Note decodeFullNote
    , D.map NoteText decodeLightNote
    ]

decoderNoteIdVersion : D.Decoder NoteIdVersion
decoderNoteIdVersion =
  D.map2
    NoteIdVersion
    (D.field "noteId" D.int)
    (D.field "noteVersion" D.int)