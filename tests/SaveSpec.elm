module SaveSpec exposing (..)

import Test             exposing (..)


import Expect      exposing (Expectation)
import FP          exposing (const)
import Save        exposing (isBusy, WhatAreWeDoing(..), viewNewNoteButton, NoteWithContent(..), viewDeleteNoteButton)

import Test.Html.Selector as Selector
import List.Nonempty      as NE
import Note               as SC
import Json.Encode        as E
import Json.Decode        as D

import Test.Html.Query as Query


busyStates : NE.Nonempty WhatAreWeDoing
busyStates =
  NE.Nonempty SavingNoteRemotely
    [
      UpdatingSessionCache
    , DeletingNoteRemotely
    , DeletingNoteFromSessionCache
    ]

allStates : NE.Nonempty WhatAreWeDoing
allStates = NE.fromElement Idle |> NE.append busyStates


whatAreWeDoingString : WhatAreWeDoing -> String
whatAreWeDoingString doing =
  case doing of
    SavingNoteRemotely            -> "SavingNoteRemotely"
    SavingNoteLocally             -> "SavingNoteLocally"
    UpdatingSessionCache          -> "UpdatingSessionCache"
    DeletingNoteRemotely          -> "DeletingNoteRemotely"
    DeletingNoteFromSessionCache  -> "DeletingNoteFromSessionCache"
    Idle                          -> "Idle"


isBusyWhenBusy : Test
isBusyWhenBusy =
  test "Returns True when busy" <|
    const <| combineExpectationsWith expectToBeBusy busyStates


expectToBeBusy : WhatAreWeDoing -> Expectation
expectToBeBusy doing = Expect.true ("is busy shouldn't have been True for " ++ (whatAreWeDoingString doing)) <| isBusy doing


isBusyWhenIdle : Test
isBusyWhenIdle =
  test "Returns False when idle" <|
    const <|
      let actual   = isBusy Idle
          expected = False
      in Expect.equal actual expected


viewNewNoteButtonWhenIdle : Test
viewNewNoteButtonWhenIdle =
  test "Should be enabled when idle" <|
    \_ ->
      viewNewNoteButton Idle
        |> Query.fromHtml
        |> Query.hasNot [Selector.class "is-static"]


viewNewNoteButtonWhenBusy : Test
viewNewNoteButtonWhenBusy =
  test "Should be disabled when busy" <|
    const <| combineExpectationsWith expectViewNoteButtonIsDisabled busyStates


expectViewNoteButtonIsDisabled : WhatAreWeDoing -> Expectation
expectViewNoteButtonIsDisabled doing =
  let html = viewNewNoteButton doing
  in Query.has [Selector.class "is-static"] <| Query.fromHtml html



viewDeleteNoteButtonWithEmptyNote : Test
viewDeleteNoteButtonWithEmptyNote =
  test "Should disable the delete button when the note is new and has no content in any application state" <|
    \_ ->
      let unsavedNote = NoteWithoutId <| SC.mkLightNote ""
      in combineExpectationsWith (expectDeleteNoteButtonIsDisabled unsavedNote) allStates


viewDeleteNoteButtonWithNewNote : Test
viewDeleteNoteButtonWithNewNote =
  test "Should disable the delete button when the note has content and is unsaved in any application state" <|
    \_ ->
      let unsavedNote = NoteWithoutId <| SC.mkLightNote "this is a new one"
      in combineExpectationsWith (expectDeleteNoteButtonIsDisabled unsavedNote) allStates


viewDeleteNoteButtonWithAnyNoteWhenBusy : Test
viewDeleteNoteButtonWithAnyNoteWhenBusy =
  test "Should disable the delete button any application busy state" <|
    \_ ->
      let unsavedNote       = NoteWithoutId <| SC.mkLightNote "this is a new one"
          unsavedNoteCombos = combineExpectationsWith (expectDeleteNoteButtonIsDisabled unsavedNote) busyStates
          savedNoteCombos   = combineExpectationsWith (expectDeleteNoteButtonIsDisabledOnFullNote createSavedNote) busyStates
      in combineExpectations <| NE.Nonempty unsavedNoteCombos [ savedNoteCombos ]


expectDeleteNoteButtonIsDisabledOnFullNote : Result D.Error SC.NoteFull -> WhatAreWeDoing -> Expectation
expectDeleteNoteButtonIsDisabledOnFullNote fullNoteResult doing =
  case fullNoteResult of
    Ok fullNote -> expectDeleteNoteButtonIsDisabled (NoteWithId fullNote) doing
    Err e       -> Expect.fail <| "Could not decode fullNote because: " ++ (D.errorToString e)


-- This is the only way to create an instance of this opaque type.
createSavedNote : Result D.Error SC.NoteFull
createSavedNote =
  let noteJson =
        E.object
         [
            ("noteText", E.string "Some text")
          , ("noteId", E.int 1000)
          , ("noteVersion", E.int 1)
         ]
  in D.decodeValue SC.decodeFullNote noteJson


expectDeleteNoteButtonIsDisabled : NoteWithContent -> WhatAreWeDoing -> Expectation
expectDeleteNoteButtonIsDisabled note doing =
  let html = viewDeleteNoteButton doing note
  in Query.has [Selector.class "is-static"] <| Query.fromHtml html

--viewDeleteNoteButton : WhatAreWeDoing -> NoteWithContent -> Html Msg
--viewDeleteNoteButton doing note =
--  let showSpinner = isBusy doing
--  in
--    button
--      [
--        id "delete-note"
--         , onClick DeleteNoteMsg
--         , classList
--             [
--               ("button", True)
--             , ("level-item", True)
--             , ("is-danger", True)
--             , ("mt-1", True)
--             , ("is-static", not (hasContent note) || showSpinner)
--             ]
--       ]
--       [text "Delete"]

-- Utils


combineExpectationsWith : (a -> Expectation) -> NE.Nonempty a -> Expectation
combineExpectationsWith assertionF values =
  combineExpectations <| NE.map assertionF values


combineExpectations : NE.Nonempty Expectation -> Expectation
combineExpectations nel =
  let expectations : NE.Nonempty (a -> Expectation)
      expectations = NE.map const nel
  in  Expect.all (NE.toList expectations) ()
