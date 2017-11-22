module Data.Taskell.Seq where

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (!?), insertAt, deleteAt)

extract :: Int -> Seq a -> Maybe (Seq a, a)
extract i xs = do
    c <- xs !? i
    let a = deleteAt i xs
    return (a, c)

update :: Int -> Seq a -> a -> Seq a
update i xs x = insertAt i x $ deleteAt i xs

shiftBy' :: Int -> Int -> Seq a  -> Maybe (Seq a)
shiftBy' from dir xs = do
    current <- xs !? from
    let r = deleteAt from xs
    return $ insertAt (from + dir) current r

shiftBy :: Int -> Int -> Seq a -> Seq a
shiftBy from dir xs = fromMaybe xs (shiftBy' from dir xs)
