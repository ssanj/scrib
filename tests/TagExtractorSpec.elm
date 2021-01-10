module TagExtractorSpec exposing (..)

import Test exposing (..)

import Expect      exposing (Expectation)
--import Fuzz        exposing (Fuzzer, int, list, string)

import TagExtractor exposing (..)

handleTitleWithoutATag : Test
handleTitleWithoutATag =
  test "Can handle a title without any tags" <|
    \_ ->
      let title    = "# Some title without tags"
          actual   = extractTags title
          expected = [ TitleText "# Some title without tags" ]
      in Expect.equal actual expected

handleTitleWithATag : Test
handleTitleWithATag =
  test "Can handle a title with a tag" <|
    \_ ->
      let title    = "# Some title [my tag]"
          actual   = extractTags title
          expected = [ TitleText "# Some title ", TitleTag "my tag" ]
      in Expect.equal actual expected

handleTitleWithoutAClosingTagBrace : Test
handleTitleWithoutAClosingTagBrace =
  test "Can handle a title with a tag without a closing brace" <|
    \_ ->
      let title    = "# Some title [my tag more content"
          actual   = extractTags title
          expected = [ TitleText "# Some title ", TitleTag "my tag more content" ]
      in Expect.equal actual expected

handleTitleWithoutAOpeningTagBrace : Test
handleTitleWithoutAOpeningTagBrace =
  test "Can handle a title with a tag without an opening brace" <|
    \_ ->
      let title    = "# Some title my tag] more content"
          actual   = extractTags title
          expected = [ TitleText "# Some title my tag] more content" ]
      in Expect.equal actual expected

handleTitleWithMultipleTags : Test
handleTitleWithMultipleTags =
  test "Can handle a title with many tags" <|
    \_ ->
      let title    = "# [Blog] testing in elm [test] with elm-test"
          actual   = extractTags title
          expected = [ TitleText "# ", TitleTag "Blog", TitleText " testing in elm ", TitleTag "test", TitleText " with elm-test" ]
      in Expect.equal actual expected

