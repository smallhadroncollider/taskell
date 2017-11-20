{-# LANGUAGE DeriveGeneric #-}

module Data.Taskell.Task where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Prelude hiding (filter)
import Data.Sequence (Seq, (|>), (!?), fromList, filter, deleteAt)

data Task = Task {
    description :: String,
    completed :: Bool
} deriving (Generic, Show, Eq)

instance ToJSON Task
instance FromJSON Task

swap :: Task -> Task
swap t = t { completed = (not (completed t)) }


-- a list of tasks
type Tasks = Seq Task

empty :: Tasks
empty = fromList []

extract :: Int -> Tasks -> Maybe (Tasks, Task)
extract i ts = do
    c <- ts !? i
    let a = deleteAt i ts
    return (a, c)

reduce :: (Tasks, Tasks) -> Task -> (Tasks, Tasks)
reduce (todo, done) t
    | completed t = (todo, done |> t)
    | otherwise = (todo |> t, done)

split :: Tasks -> (Tasks, Tasks)
split = foldl reduce (empty, empty) 
