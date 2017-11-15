{-# LANGUAGE DeriveGeneric #-}

module Task where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy

data Task = Task {
    description :: String,
    completed :: Bool
} deriving (Generic, Show)

instance ToJSON Task
instance FromJSON Task

type Tasks = [Task]

jsonToTasks :: ByteString -> Maybe Tasks
jsonToTasks s = decode s :: Maybe Tasks
