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
