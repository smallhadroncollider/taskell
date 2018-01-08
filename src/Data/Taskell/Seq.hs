module Data.Taskell.Seq where

import Prelude hiding (splitAt, drop)
import Data.Sequence (Seq, (!?), (|>), (><), insertAt, deleteAt, splitAt, drop, fromList)

type Split a = (Seq a, a, Seq a)

empty :: Seq a
empty = fromList []

extract :: Int -> Seq a -> Maybe (Seq a, a)
extract i xs = do
    c <- xs !? i
    let a = deleteAt i xs
    return (a, c)

splitOn :: Int -> Seq a -> Maybe (Split a)
splitOn i xs = do
    let (a, b) = splitAt i xs
    current <- b !? 0
    let b' = drop 1 b
    return (a, current, b')

update :: Int -> Seq a -> a -> Seq a
update i xs x = insertAt i x $ deleteAt i xs

updateFn :: Int -> (a -> a) -> Seq a -> Maybe (Seq a)
updateFn i fn xs = do
    (a, c, b) <- splitOn i xs
    return ((a |> fn c) >< b)

shiftBy :: Int -> Int -> Seq a  -> Maybe (Seq a)
shiftBy from dir xs | from == 0 && dir < 0 = Nothing
                    | otherwise = do
                        current <- xs !? from
                        let r = deleteAt from xs
                        return $ insertAt (from + dir) current r
