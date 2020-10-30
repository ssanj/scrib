module FP exposing (..)

maybe : b -> (a -> b) -> Maybe a -> b
maybe onNothing onJust maybeVal =
  case maybeVal of
    (Just a) -> onJust a
    Nothing  -> onNothing

const : a -> b -> a
const a _ = a

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

maybeToBool : Maybe a -> Bool
maybeToBool = maybe False (const True)

collect : (a -> Maybe b) -> List a -> List b
collect predicate elements =
  let mapped = List.map predicate elements
  in List.concatMap maybeToList mapped

maybeToList : Maybe a -> List a
maybeToList = maybe [] List.singleton


find : (a -> Bool) -> List a -> Maybe a
find predicate elements =
  case elements of
    [] -> Nothing
    (x::xs) -> if predicate x then Just x else find predicate xs