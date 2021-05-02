module TagExtractor exposing (..)

import FP exposing (partitionExcluding)


type TitleType = TitleText String | TitleTag String

-- Can we replace this with a parser?
extractTags : String -> List TitleType
extractTags title =
  let
      extractTitleTypes : List Char -> List TitleType
      extractTitleTypes titleChars =
        let (prefix, prefixRem) = partitionExcluding (\c -> c /= '[') titleChars
            (tag, tagRem) = partitionExcluding (\c -> c /= ']') prefixRem
            restOfList   = if List.isEmpty tagRem then [] else extractTitleTypes tagRem
        in case (prefix, tag) of
            ([], [])            -> restOfList
            (hasPrefix, [])     -> TitleText (String.fromList hasPrefix) :: restOfList
            ([], hasTag)        -> TitleTag (String.fromList hasTag) :: restOfList
            (hasPrefix, hasTag) -> (TitleText (String.fromList hasPrefix) :: TitleTag (String.fromList hasTag) :: restOfList)
  in extractTitleTypes (String.toList title)


isTag : TitleType -> Bool
isTag titleType =
  case titleType of
    TitleText _ -> False
    TitleTag  _ -> True

isText : TitleType -> Bool
isText = not << isTag


getStringTags : TitleType -> Maybe String
getStringTags titleType =
  case titleType of
    TitleText value -> Just value
    TitleTag _      -> Nothing

getTags : TitleType -> Maybe String
getTags titleType =
  case titleType of
    TitleText _    -> Nothing
    TitleTag value -> Just value

