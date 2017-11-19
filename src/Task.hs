{-# LANGUAGE DeriveGeneric #-}

module Task where

import GHC.Generics
import Data.Aeson
import Data.Sequence (Seq, fromList)

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
