{-# LANGUAGE NoImplicitPrelude #-}
module Data.Taskell.Seq where

import ClassyPrelude

import Data.Sequence ((!?), insertAt, deleteAt)

extract :: Int -> Seq a -> Maybe (Seq a, a)
extract i xs = do
    c <- xs !? i
    let a = deleteAt i xs
    return (a, c)

shiftBy :: Int -> Int -> Seq a  -> Maybe (Seq a)
shiftBy from dir xs = do
    current <- xs !? from
    let r = deleteAt from xs
    return $ insertAt (from + dir) current r
