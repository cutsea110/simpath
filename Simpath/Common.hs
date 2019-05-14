module Simpath.Common where

justIf :: Bool -> a -> Maybe a
justIf b a = if b then Just a else Nothing

mapOrElse :: (a -> b) -> b -> Maybe a -> b
mapOrElse f b ma = case ma of { Just a -> f a; _ -> b }
