module FP exposing (..)

maybe : b -> (a -> b) -> Maybe a -> b
maybe onNothing onJust maybeVal =
  case maybeVal of
    (Just a) -> onJust a
    Nothing  -> onNothing

const : a -> b -> a
const a _ = a

