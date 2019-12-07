module Minesweeper.Utils where

applyIf :: (a -> a) -> a -> Bool -> a
applyIf f x apply = if apply then f x else x

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

zipTuplesWith :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipTuplesWith f (a, b) (c, d) = (f a c, f b d)
