{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Task.Internal where

import ClassyPrelude

import Control.Lens (makeLenses, (&), (^.), (^?), (.~), (%~), ix)

import Data.Sequence as S ((|>), adjust', deleteAt)
import Data.Text (strip)
import Data.Taskell.Date (Day, textToDay)
import qualified Data.Taskell.Subtask as ST (Subtask, Update, name, complete)

data Task = Task {
    _name :: Text,
    _description :: Maybe Text,
    _subtasks :: Seq ST.Subtask,
    _due :: Maybe Day
} deriving (Show, Eq)

type Update = Task -> Task

-- create lenses
$(makeLenses ''Task)


-- operations
blank :: Task
blank = Task "" Nothing empty Nothing

new :: Text -> Task
new text = blank & (name .~ text)

setDescription :: Text -> Update
setDescription text =
    if null (strip text)
        then id
        else description .~ Just text

setDue :: Text -> Update
setDue date =
    case textToDay date of
        Just day -> due .~ Just day
        Nothing -> id

getSubtask :: Int -> Task -> Maybe ST.Subtask
getSubtask idx = (^? subtasks . ix idx)

addSubtask :: ST.Subtask -> Update
addSubtask subtask = subtasks %~ (|> subtask)

hasSubtasks :: Task -> Bool
hasSubtasks = not . null . (^. subtasks)

updateSubtask :: Int -> ST.Update -> Update
updateSubtask idx fn = subtasks %~ adjust' fn idx

removeSubtask :: Int -> Update
removeSubtask idx = subtasks %~ S.deleteAt idx

countSubtasks :: Task -> Int
countSubtasks = length . (^. subtasks)

countCompleteSubtasks :: Task -> Int
countCompleteSubtasks = length . filter (^. ST.complete) . (^. subtasks)

contains :: Text -> Task -> Bool
contains text task =
    text `isInfixOf` (task ^. name) || not (null sts)
    where sts = filter (isInfixOf text) $ (^. ST.name) <$> (task ^. subtasks)

isBlank :: Task -> Bool
isBlank = null . (^. name)
