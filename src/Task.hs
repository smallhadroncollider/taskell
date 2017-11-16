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

-- a list of tasks
type Tasks = [Task]

-- return tasks or empty list
deMaybe :: Maybe Tasks -> Tasks
deMaybe (Just ts) = ts
deMaybe Nothing = []

-- returns a Maybe
jsonToTasks' :: ByteString -> Maybe Tasks
jsonToTasks' s = decode s :: Maybe Tasks

-- returns tasks or an empty list
jsonToTasks :: ByteString -> Tasks
jsonToTasks = deMaybe . jsonToTasks'

-- tasks to JSON
tasksToJSON :: Tasks -> ByteString
tasksToJSON = encode
