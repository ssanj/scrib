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


isBusyWhenBusy : Test
isBusyWhenBusy =
  test "Returns True when busy" <|
    \_ ->
      let expected = True
          expectToBeBusy : WhatAreWeDoing -> Expectation
          expectToBeBusy doing = Expect.true ("is busy shouldn't have been True for " ++ (whatAreWeDoingString doing)) <| isBusy doing

          allExpectations = NE.map expectToBeBusy busyStates
      in combineExpectations allExpectations


isBusyWhenIdle : Test
isBusyWhenIdle =
  test "Returns False when idle" <|
    \_ ->
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
    \_ ->
      let expectViewNoteButtonIsDisabled :  WhatAreWeDoing -> Expectation
          expectViewNoteButtonIsDisabled doing =
            let html = viewNewNoteButton doing
            in Query.has [Selector.class "is-static"] <| Query.fromHtml html
          allExpectations = NE.map expectViewNoteButtonIsDisabled busyStates
      in combineExpectations allExpectations


combineExpectations : NE.Nonempty Expectation -> Expectation
combineExpectations nel =
  let expectations : NE.Nonempty (a -> Expectation)
      expectations = NE.map const nel
  in  Expect.all (NE.toList expectations) ()
