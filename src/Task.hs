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

deMaybe :: Maybe Tasks -> Tasks
deMaybe (Just ts) = ts
deMaybe Nothing = []

jsonToTasks' :: ByteString -> Maybe Tasks
jsonToTasks' s = decode s :: Maybe Tasks

jsonToTasks :: ByteString -> Tasks
jsonToTasks = deMaybe . jsonToTasks'
