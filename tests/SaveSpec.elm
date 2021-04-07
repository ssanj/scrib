module SaveSpec exposing (..)

import Test exposing (..)

import Expect      exposing (Expectation)
import FP          exposing (const)
import Save        exposing (isBusy, WhatAreWeDoing(..), viewNewNoteButton)
import Test.Html.Selector as Selector

import Test.Html.Query as Query


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
      let html = viewNewNoteButton SavingNoteRemotely
      in Query.has [Selector.class "is-static"] <| Query.fromHtml html