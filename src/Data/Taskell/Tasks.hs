{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Tasks where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, withObject, object, (.:), (.=))

import Data.Sequence (Seq, (|>), (!?), deleteAt)
import qualified Data.Taskell.Seq as S

import Data.Taskell.Task (Task, blank)

data Tasks = Tasks String (Seq Task) deriving (Show, Eq)

instance FromJSON Tasks where
    parseJSON = withObject "Tasks" $ \obj -> do
        title <- obj .: "title" 
        tasks <- obj .: "tasks" 
        return (Tasks title tasks)

instance ToJSON Tasks where
    toJSON (Tasks title ts) = object ["title" .= title, "tasks" .= ts]

-- useful functions
empty :: String -> Tasks
empty t = Tasks t S.empty 

new :: Tasks -> Tasks
new (Tasks title ts) = Tasks title (ts |> blank)

extract :: Int -> Tasks -> Maybe (Tasks, Task)
extract i (Tasks title ts) = do
    (xs, x) <- S.extract i ts
    return (Tasks title xs, x)

update :: Int -> (Task -> Task) -> Tasks -> Tasks
update i fn (Tasks title ts) = Tasks title $ S.updateFn i fn ts

move :: Int -> Int -> Tasks -> Tasks
move from dir (Tasks title ts) = Tasks title $ S.shiftBy from dir ts

deleteTask :: Int -> Tasks -> Tasks
deleteTask i (Tasks title ts) = Tasks title (deleteAt i ts)

getTask :: Int -> Tasks -> Maybe Task
getTask i (Tasks _ ts) = ts !? i
