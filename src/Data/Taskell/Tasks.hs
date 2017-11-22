{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Tasks where

import Data.Maybe (fromMaybe)
import Data.Aeson
import Prelude hiding (splitAt, drop)
import Data.Sequence (Seq, (><), (|>), (!?), fromList, insertAt, deleteAt, splitAt, drop)

import Data.Taskell.Task (Task)

data Tasks = Tasks String (Seq Task) deriving (Show, Eq)

instance FromJSON Tasks where
    parseJSON = withObject "Tasks" $ \obj -> do
        title <- obj .: "title" 
        tasks <- obj .: "tasks" 
        return (Tasks title tasks)

instance ToJSON Tasks where
    toJSON (Tasks title ts) = object ["title" .= title, "tasks" .= ts]

empty :: String -> Tasks
empty t = Tasks t (fromList [])

extract :: Int -> Tasks -> Maybe (Tasks, Task)
extract i (Tasks title ts) = do
    c <- ts !? i
    let a = deleteAt i ts
    return (Tasks title a, c)

update' :: Int -> (Task -> Task) -> Tasks -> Maybe Tasks
update' i fn (Tasks title ts) = do 
    let (a, b) = splitAt i ts 
    current <- b !? 0
    let b' = drop 1 b
    let items = (a |> fn current) >< b'
    return (Tasks title items)

update :: Int -> (Task -> Task) -> Tasks -> Tasks
update i fn tasks = fromMaybe tasks (update' i fn tasks)

move' :: Int -> Int -> Tasks  -> Maybe Tasks
move' from dir (Tasks title ts) = do
    current <- ts !? from
    let r = deleteAt from ts
    let items = insertAt (from + dir) current r
    return (Tasks title items)

move :: Int -> Int -> Tasks -> Tasks
move from dir tasks = fromMaybe tasks (move' from dir tasks)
