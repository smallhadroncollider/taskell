{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.List where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, withObject, object, (.:), (.=))

import Data.Sequence (Seq, (|>), (!?), deleteAt)
import qualified Data.Taskell.Seq as S

import Data.Taskell.Task (Task, blank)

data List = List String (Seq Task) deriving (Show, Eq)

instance FromJSON List where
    parseJSON = withObject "List" $ \obj -> do
        title <- obj .: "title" 
        tasks <- obj .: "tasks" 
        return (List title tasks)

instance ToJSON List where
    toJSON (List title ts) = object ["title" .= title, "tasks" .= ts]

-- useful functions
empty :: String -> List
empty t = List t S.empty 

new :: List -> List
new = append blank

append :: Task -> List -> List
append t (List title ts) = List title (ts |> t)

extract :: Int -> List -> Maybe (List, Task)
extract i (List title ts) = do
    (xs, x) <- S.extract i ts
    return (List title xs, x)

update :: Int -> (Task -> Task) -> List -> Maybe List
update i fn (List title ts) = do
    ts' <- S.updateFn i fn ts
    return $ List title ts'

move :: Int -> Int -> List -> Maybe List
move from dir (List title ts) = do
    ts' <- S.shiftBy from dir ts
    return $ List title ts'

deleteTask :: Int -> List -> List
deleteTask i (List title ts) = List title (deleteAt i ts)

getTask :: Int -> List -> Maybe Task
getTask i (List _ ts) = ts !? i
