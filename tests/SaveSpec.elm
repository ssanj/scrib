module SaveSpec exposing (..)

import Test exposing (..)

import Expect      exposing (Expectation)
import FP          exposing (const)
import Save        exposing (isBusy, WhatAreWeDoing(..))

isBusyWhenBusy : Test
isBusyWhenBusy =
  test "Returns True when busy" <|
    \_ ->
      let actualList   = List.map isBusy
                          [
                            SavingNoteRemotely
                          , UpdatingSessionCache
                          , DeletingNoteRemotely
                          , DeletingNoteFromSessionCache
                          ]
          expected = True
      in Expect.equal (List.all (const True) actualList) expected

isBusyWhenIdle : Test
isBusyWhenIdle =
  test "Returns False when idle" <|
    \_ ->
      let actual   = isBusy Idle
          expected = False
      in Expect.equal actual expected

