module SaveSpec exposing (..)

import Test             exposing (..)


import Expect      exposing (Expectation)
import FP          exposing (const)
import Save        exposing (isBusy, WhatAreWeDoing(..), viewNewNoteButton)

import Test.Html.Selector as Selector
import List.Nonempty      as NE

import Test.Html.Query as Query


busyStates : NE.Nonempty WhatAreWeDoing
busyStates =
  NE.Nonempty SavingNoteRemotely
    [
      UpdatingSessionCache
    , DeletingNoteRemotely
    , DeletingNoteFromSessionCache
    ]


whatAreWeDoingString : WhatAreWeDoing -> String
whatAreWeDoingString doing =
  case doing of
    SavingNoteRemotely            -> "SavingNoteRemotely"
    SavingNoteLocally             -> "SavingNoteLocally"
    UpdatingSessionCache          -> "UpdatingSessionCache"
    DeletingNoteRemotely          -> "DeletingNoteRemotely"
    DeletingNoteFromSessionCache  -> "DeletingNoteFromSessionCache"
    Idle                          -> "Idle"


expectToBeBusy : WhatAreWeDoing -> Expectation
expectToBeBusy doing = Expect.true ("is busy shouldn't have been True for " ++ (whatAreWeDoingString doing)) <| isBusy doing

isBusyWhenBusy : Test
isBusyWhenBusy =
  test "Returns True when busy" <|
    const <| combineExpectationsWith expectToBeBusy busyStates


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


expectViewNoteButtonIsDisabled :  WhatAreWeDoing -> Expectation
expectViewNoteButtonIsDisabled doing =
  let html = viewNewNoteButton doing
  in Query.has [Selector.class "is-static"] <| Query.fromHtml html


viewNewNoteButtonWhenBusy : Test
viewNewNoteButtonWhenBusy =
  test "Should be disabled when busy" <|
    const <| combineExpectationsWith expectViewNoteButtonIsDisabled busyStates


combineExpectationsWith : (a -> Expectation) -> NE.Nonempty a -> Expectation
combineExpectationsWith assertionF values =
  combineExpectations <| NE.map assertionF values


combineExpectations : NE.Nonempty Expectation -> Expectation
combineExpectations nel =
  let expectations : NE.Nonempty (a -> Expectation)
      expectations = NE.map const nel
  in  Expect.all (NE.toList expectations) ()
