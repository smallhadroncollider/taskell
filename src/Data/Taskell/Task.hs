{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Task where

import ClassyPrelude

import Data.Sequence as S ((|>), (!?), adjust', deleteAt)
import Data.Text (strip)
import Data.Taskell.Date (Day, textToDay)

data SubTask = SubTask {
    name :: Text,
    complete :: Bool
} deriving (Show, Eq)

data Task = Task {
    description :: Text,
    summary :: Maybe Text,
    subTasks :: Seq SubTask,
    due :: Maybe Day
} deriving (Show, Eq)

blank :: Task
blank = Task {
        description = "",
        summary = Nothing,
        subTasks = empty,
        due = Nothing
    }

new :: Text -> Task
new s = blank { description = s }

setSummary :: Text -> Task -> Task
setSummary text task = if null (strip text) then task else task { summary = Just text }

setDue :: Text -> Task -> Task
setDue date task = task { due = textToDay date}

blankSubTask :: SubTask
blankSubTask = SubTask { name = "", complete = False }

subTask :: Text -> Bool -> SubTask
subTask n c = SubTask { name = n, complete = c }

getSubTask :: Int -> Task -> Maybe SubTask
getSubTask i task = subTasks task !? i

addSubTask :: SubTask -> Task -> Task
addSubTask s t = t { subTasks = subTasks t |> s }

setSubTaskName :: Text -> SubTask -> SubTask
setSubTaskName text subtask = subtask { name = text }

hasSubTasks :: Task -> Bool
hasSubTasks t = not (null (subTasks t))

updateSubTask :: Int -> (SubTask -> SubTask) -> Task -> Task
updateSubTask i fn task = task { subTasks = sts }
    where sts = adjust' fn i (subTasks task)

removeSubTask :: Int -> Task -> Task
removeSubTask i task = task { subTasks = S.deleteAt i (subTasks task) }

toggleComplete :: SubTask -> SubTask
toggleComplete st = st { complete = not (complete st) }

countSubTasks :: Task -> Int
countSubTasks = length . subTasks

countCompleteSubTasks :: Task -> Int
countCompleteSubTasks = length . filter complete . subTasks

contains :: Text -> Task -> Bool
contains s t = s `isInfixOf` description t || not (null sts)
    where sts = filter (isInfixOf s) $ name <$> subTasks t

isBlank :: Task -> Bool
isBlank t = null $ description t
