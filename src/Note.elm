module Note exposing
  (
    -- DATA TYPES

    NoteLight
  , NoteFull
  , NoteIdVersion

    -- CONSTRUCTORS

  , mkLightNote
  , mkFullNote

    -- FOLDS

  --, foldNote

    -- GETTERS

  , getNoteLightText
  , getNoteFullText
  , getNoteIdNoteFull
  , getNoteVersionNoteFull

    -- UPDATES

  , updateNoteIdVersion
  , updateNoteVersion
  , updateNoteLightText
  , updateNoteFullText

    -- ENCODERS

  , encodeLightNote
  , encodeFullNote
  , encodeFullNotes

    -- DECODERS

  , decoderNoteIdVersion
  --, decodeNote
  , decodeLightNote
  , decodeFullNote
  , decodeFullNotes
   )

import ElmCommon exposing (Encoder)
import Json.Encode as E
import Json.Decode as D


-- MODEL OPAQUE


type NoteFull = NoteFull String NoteIdVersion

type NoteLight = NoteLight String


-- MODEL OPEN


type alias NoteIdVersion = { noteId : Int, noteVersion : Int }


-- CONSTRUCTORS


mkLightNote : String -> NoteLight
mkLightNote = NoteLight

mkFullNote : String -> NoteIdVersion -> NoteFull
mkFullNote  noteText idVersion = NoteFull noteText idVersion


-- FOLDS


--foldNote : (NoteLight -> a) -> (NoteFull -> a ) -> Note -> a
--foldNote onNoteLight onNoteFull note =
--  case note of
--    Note noteFull      -> onNoteFull noteFull
--    NoteText noteLight -> onNoteLight noteLight


-- GETTERS


getNoteLightText : NoteLight -> String
getNoteLightText (NoteLight noteText) = noteText

getNoteFullText : NoteFull -> String
getNoteFullText (NoteFull noteText _) = noteText

--getNoteText : Note -> String
--getNoteText note =
--  case note of
--    (Note (NoteFull noteText _)) -> noteText
--    (NoteText (NoteLight noteText))  -> noteText

--getNoteId : Note -> Maybe Int
--getNoteId note =
--  case note of
--    (Note (NoteFull _ { noteId })) -> Just noteId
--    _                 -> Nothing

--getNoteVersion : Note -> Maybe Int
--getNoteVersion note =
--  case note of
--    (Note (NoteFull _ { noteVersion })) -> Just noteVersion
--    _                                   -> Nothing

getNoteIdNoteFull : NoteFull -> Int
getNoteIdNoteFull (NoteFull _ { noteId }) = noteId

getNoteVersionNoteFull : NoteFull -> Int
getNoteVersionNoteFull (NoteFull _ { noteVersion }) = noteVersion


-- UPDATES


updateNoteLightText : String -> NoteLight
updateNoteLightText = mkLightNote

updateNoteFullText : String -> NoteFull -> NoteFull
updateNoteFullText newText (NoteFull _ idVersion) = NoteFull newText idVersion

updateNoteIdVersion : NoteIdVersion -> NoteLight -> NoteFull
updateNoteIdVersion idVersion (NoteLight noteText) = NoteFull noteText idVersion

updateNoteVersion : NoteIdVersion -> NoteFull -> NoteFull
updateNoteVersion { noteVersion } (NoteFull noteText {noteId}) =  NoteFull noteText { noteId = noteId, noteVersion = noteVersion }


-- ENCODERS


--encodeNote: Note -> E.Value
--encodeNote note =
--  case note of
--    (Note noteFull)      -> encodeFullNote noteFull
--    (NoteText noteLight) -> encodeLightNote noteLight

encodeFullNotes: Encoder (List NoteFull)
encodeFullNotes = E.list encodeFullNote

encodeFullNote: Encoder NoteFull
encodeFullNote (NoteFull noteText {noteId, noteVersion}) =
  E.object
   [
      ("noteText", E.string noteText)
    , ("noteId", E.int noteId)
    , ("noteVersion", E.int noteVersion)
   ]

encodeLightNote: Encoder NoteLight
encodeLightNote  (NoteLight noteText) =
  E.object
   [
      ("noteText", E.string noteText)
   ]


-- DECODERS


decodeFullNotes: D.Decoder (List NoteFull)
decodeFullNotes = D.list decodeFullNote

decodeFullNote: D.Decoder NoteFull
decodeFullNote =
  D.map2
    NoteFull
    (D.field "noteText" D.string)
    decoderNoteIdVersion

decodeLightNote: D.Decoder NoteLight
decodeLightNote = D.map NoteLight (D.field "noteText" D.string)

--decodeNote : D.Decoder Note
--decodeNote =
--  D.oneOf
--    [
--      D.map Note decodeFullNote
--    , D.map NoteText decodeLightNote
--    ]

decoderNoteIdVersion : D.Decoder NoteIdVersion
decoderNoteIdVersion =
  D.map2
    NoteIdVersion
    (D.field "noteId" D.int)
    (D.field "noteVersion" D.int)