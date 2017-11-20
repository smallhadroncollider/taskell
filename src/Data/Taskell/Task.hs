{-# LANGUAGE DeriveGeneric #-}

module Data.Taskell.Task where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Prelude hiding (filter, splitAt, drop)
import Data.Sequence (Seq, (><), (|>), (!?), fromList, filter, deleteAt, splitAt, drop)

data Task = Task {
    description :: String,
    completed :: Bool
} deriving (Generic, Show, Eq)

instance ToJSON Task
instance FromJSON Task

blank :: Task
blank = Task { description = "", completed = False }

swap :: Task -> Task
swap t = t { completed = (not (completed t)) }

append :: Char -> Task  -> Task
append c t = t { description = (description t ++ [c]) }

backspace :: Task -> Task
backspace t = t { description = d' }
    where d = description t
          d' = if length d > 0 then init d else d

-- a list of tasks
type Tasks = Seq Task

empty :: Tasks
empty = fromList []

extract :: Int -> Tasks -> Maybe (Tasks, Task)
extract i ts = do
    c <- ts !? i
    let a = deleteAt i ts
    return (a, c)

update' :: Int -> Tasks -> (Task -> Task) -> Maybe Tasks
update' i ts fn = do 
    let (a, b) = splitAt i ts 
    current <- b !? 0
    let b' = drop 1 b
    return ((a |> fn current) >< b')

update :: Int -> Tasks -> (Task -> Task) -> Tasks
update i ts fn = maybe ts id (update' i ts fn)

reduce :: (Tasks, Tasks) -> Task -> (Tasks, Tasks)
reduce (todo, done) t
    | completed t = (todo, done |> t)
    | otherwise = (todo |> t, done)

split :: Tasks -> (Tasks, Tasks)
split = foldl reduce (empty, empty) 
