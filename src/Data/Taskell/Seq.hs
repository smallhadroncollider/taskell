module Data.Taskell.Seq where

import Data.Maybe (fromMaybe)
import Prelude hiding (splitAt, drop)
import Data.Sequence (Seq, (!?), (|>), (><), insertAt, deleteAt, splitAt, drop, fromList)

empty :: Seq a
empty = fromList []

extract :: Int -> Seq a -> Maybe (Seq a, a)
extract i xs = do
    c <- xs !? i
    let a = deleteAt i xs
    return (a, c)

update :: Int -> Seq a -> a -> Seq a
update i xs x = insertAt i x $ deleteAt i xs

updateFn :: Int -> (a -> a) -> Seq a -> Maybe (Seq a)
updateFn i fn xs = do 
    let (a, b) = splitAt i xs
    current <- b !? 0
    let b' = drop 1 b
    return ((a |> fn current) >< b') 

shiftBy :: Int -> Int -> Seq a  -> Maybe (Seq a)
shiftBy from dir xs = do
    current <- xs !? from
    let r = deleteAt from xs
    return $ insertAt (from + dir) current r
