{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Task where

import Data.Sequence as S (Seq, null, (|>), (!?), adjust', empty, length, filter, deleteAt)
import Data.Text as T (Text, length, null, isInfixOf, empty)

data SubTask = SubTask {
    name :: Text,
    complete :: Bool
} deriving (Show, Eq)

data Task = Task {
    description :: Text,
    subTasks :: Seq SubTask
} deriving (Show, Eq)

blank :: Task
blank = Task { description = T.empty, subTasks = S.empty }

clear :: Task -> Task
clear _ = blank

new :: Text -> Task
new s = blank { description = s }

blankSubTask :: SubTask
blankSubTask = SubTask { name = "", complete = False }

subTask :: Text -> Bool -> SubTask
subTask n c = SubTask { name = n, complete = c }

getSubTask :: Int -> Task -> Maybe SubTask
getSubTask index task = subTasks task !? index

addSubTask :: SubTask -> Task -> Task
addSubTask s t = t { subTasks = subTasks t |> s }

setSubTaskName :: Text -> SubTask -> SubTask
setSubTaskName text subtask = subtask { name = text }

hasSubTasks :: Task -> Bool
hasSubTasks t = not (S.null (subTasks t))

updateSubTask :: Int -> (SubTask -> SubTask) -> Task -> Task
updateSubTask index fn task = task { subTasks = sts }
    where sts = adjust' fn index (subTasks task)

removeSubTask :: Int -> Task -> Task
removeSubTask index task = task { subTasks = S.deleteAt index (subTasks task) }

toggleComplete :: SubTask -> SubTask
toggleComplete st = st { complete = not (complete st) }

countSubTasks :: Task -> Int
countSubTasks = S.length . subTasks

countCompleteSubTasks :: Task -> Int
countCompleteSubTasks = S.length . S.filter complete . subTasks

characters :: Task -> Int
characters = T.length . description

contains :: Text -> Task -> Bool
contains s t = s `T.isInfixOf` description t || not (S.null sts)
    where sts = S.filter (T.isInfixOf s) $ name <$> subTasks t

isBlank :: Task -> Bool
isBlank t = T.null $ description t
