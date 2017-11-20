{-# LANGUAGE DeriveGeneric #-}

module Data.Taskell.Task where

import GHC.Generics
import Data.Aeson
import Prelude hiding (filter)
import Data.Sequence (Seq, (|>), fromList, filter)

data Task = Task {
    description :: String,
    completed :: Bool
} deriving (Generic, Show, Eq)

instance ToJSON Task
instance FromJSON Task

-- a list of tasks
type Tasks = Seq Task

empty :: Tasks
empty = fromList []

reduce :: (Tasks, Tasks) -> Task -> (Tasks, Tasks)
reduce (todo, done) t
    | completed t = (todo, done |> t)
    | otherwise = (todo |> t, done)

split :: Tasks -> (Tasks, Tasks)
split = foldl reduce (empty, empty) 
