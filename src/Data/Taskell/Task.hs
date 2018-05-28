{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Task where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Sequence as S ((|>), (!?), adjust', deleteAt)
import Data.Text (strip)
import Data.Taskell.Date (Day, textToDay)
import qualified Data.Taskell.Subtask as ST (Subtask, Update, name, complete)

data Task = Task {
    description :: Text,
    summary :: Maybe Text,
    subtasks :: Seq ST.Subtask,
    due :: Maybe Day
} deriving (Show, Eq)

blank :: Task
blank = Task {
        description = "",
        summary = Nothing,
        subtasks = empty,
        due = Nothing
    }

new :: Text -> Task
new s = blank { description = s }

setSummary :: Text -> Task -> Task
setSummary text task = if null (strip text) then task else task { summary = Just text }

setDue :: Text -> Task -> Task
setDue date task = case due' of
    Just d -> task { due = Just d }
    Nothing -> task
    where due' = textToDay date

getSubtask :: Int -> Task -> Maybe ST.Subtask
getSubtask i task = subtasks task !? i

addSubtask :: ST.Subtask -> Task -> Task
addSubtask s t = t { subtasks = subtasks t |> s }

hasSubtasks :: Task -> Bool
hasSubtasks t = not (null (subtasks t))

updateSubtask :: Int -> ST.Update -> Task -> Task
updateSubtask i fn task = task { subtasks = sts }
    where sts = adjust' fn i (subtasks task)

removeSubtask :: Int -> Task -> Task
removeSubtask i task = task { subtasks = S.deleteAt i (subtasks task) }

countSubtasks :: Task -> Int
countSubtasks = length . subtasks

countCompleteSubtasks :: Task -> Int
countCompleteSubtasks = length . filter (^. ST.complete) . subtasks

contains :: Text -> Task -> Bool
contains s t = s `isInfixOf` description t || not (null sts)
    where sts = filter (isInfixOf s) $ (^. ST.name) <$> subtasks t

isBlank :: Task -> Bool
isBlank t = null $ description t
