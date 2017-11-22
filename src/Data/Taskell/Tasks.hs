module Data.Taskell.Tasks where

import Data.Maybe (fromMaybe)
import Prelude hiding (splitAt, drop)
import Data.Sequence (Seq, (><), (|>), (!?), fromList, insertAt, deleteAt, splitAt, drop)

import Data.Taskell.Task (Task)

type Tasks = Seq Task

empty :: Tasks
empty = fromList []

extract :: Int -> Tasks -> Maybe (Tasks, Task)
extract i ts = do
    c <- ts !? i
    let a = deleteAt i ts
    return (a, c)

update' :: Int -> (Task -> Task) -> Tasks -> Maybe Tasks
update' i fn ts = do 
    let (a, b) = splitAt i ts 
    current <- b !? 0
    let b' = drop 1 b
    return ((a |> fn current) >< b')

update :: Int -> (Task -> Task) -> Tasks -> Tasks
update i fn ts = fromMaybe ts (update' i fn ts)

move' :: Int -> Int -> Tasks  -> Maybe Tasks
move' from dir ts = do
    current <- ts !? from
    let r = deleteAt from ts
    return (insertAt (from + dir) current r)

move :: Int -> Int -> Tasks -> Tasks
move from dir ts = fromMaybe ts (move' from dir ts)
