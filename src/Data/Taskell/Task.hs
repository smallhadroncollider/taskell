{-# LANGUAGE DeriveGeneric #-}

module Data.Taskell.Task where

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON, ToJSON)
import Prelude hiding (splitAt, drop)
import Data.Sequence (Seq, (><), (|>), (!?), fromList, insertAt, deleteAt, splitAt, drop)

data Task = Task {
    description :: String,
    completed :: Bool
} deriving (Generic, Show, Eq)

instance ToJSON Task
instance FromJSON Task

blank :: Task
blank = Task { description = "", completed = False }

swap :: Task -> Task
swap t = t { completed = not (completed t) }

append :: Char -> Task  -> Task
append c t = t { description = description t ++ [c] }

backspace :: Task -> Task
backspace t = t { description = d' }
    where d = description t
          d' = if not (null d) then init d else d

-- a list of tasks
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

reduce :: (Tasks, Tasks) -> Task -> (Tasks, Tasks)
reduce (todo, done) t
    | completed t = (todo, done |> t)
    | otherwise = (todo |> t, done)

split :: Tasks -> (Tasks, Tasks)
split = foldl reduce (empty, empty) 
