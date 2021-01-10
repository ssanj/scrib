module TagExtractor exposing (..)

import FP exposing (partitionExcluding)


type TitleType = TitleText String | TitleTag String

extractTags : String -> List TitleType
extractTags title =
  let
      extractTitleTypes : List Char -> List TitleType
      extractTitleTypes titleChars =
        let (prefix, prefixRem) = partitionExcluding (\c -> c /= '[') titleChars
            (tag, tagRem) = partitionExcluding (\c -> c /= ']') prefixRem
            restOfList   = if List.isEmpty tagRem then [] else extractTitleTypes tagRem
        in (TitleText (String.fromList prefix) :: TitleTag (String.fromList tag) :: restOfList)
  in extractTitleTypes (String.toList title)
