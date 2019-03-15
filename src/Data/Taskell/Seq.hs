{-# LANGUAGE NoImplicitPrelude #-}

module Data.Taskell.Seq where

import ClassyPrelude

import Data.Sequence (deleteAt, insertAt, (!?))

extract :: Int -> Seq a -> Maybe (Seq a, a)
extract idx xs = (,) (deleteAt idx xs) <$> xs !? idx

shiftBy :: Int -> Int -> Seq a -> Maybe (Seq a)
shiftBy idx dir xs = do
    (a, current) <- extract idx xs
    pure $ insertAt (idx + dir) current a
