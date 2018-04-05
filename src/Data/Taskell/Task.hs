{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Task where

import ClassyPrelude

import Data.Sequence as S ((|>), (!?), adjust', deleteAt)

data SubTask = SubTask {
    name :: Text,
    complete :: Bool
} deriving (Show, Eq)

data Task = Task {
    description :: Text,
    subTasks :: Seq SubTask
} deriving (Show, Eq)

blank :: Task
blank = Task { description = "", subTasks = empty }

new :: Text -> Task
new s = blank { description = s }

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

characters :: Task -> Int
characters = length . description

contains :: Text -> Task -> Bool
contains s t = s `isInfixOf` description t || not (null sts)
    where sts = filter (isInfixOf s) $ name <$> subTasks t

isBlank :: Task -> Bool
isBlank t = null $ description t
