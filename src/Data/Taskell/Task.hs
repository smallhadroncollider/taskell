{-# LANGUAGE DeriveGeneric #-}

module Data.Taskell.Task where

import GHC.Generics
import Data.Aeson
import Prelude hiding (filter)
import Data.Sequence (Seq, fromList, filter)

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

complete :: Tasks -> Tasks 
complete = filter completed

incomplete :: Tasks -> Tasks 
incomplete = filter (not . completed)
