{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Task.Internal where

import ClassyPrelude

import Control.Lens (ix, makeLenses, (%~), (&), (.~), (?~), (^.), (^?))

import           Data.Sequence        as S (adjust', deleteAt, (|>))
import           Data.Taskell.Date    (Due (..), inputToTime)
import qualified Data.Taskell.Subtask as ST (Subtask, Update, complete, duplicate, name)
import           Data.Text            (strip)
import           Data.Time.Zones      (TZ)

data Task = Task
    { _name        :: Text
    , _description :: Maybe Text
    , _subtasks    :: Seq ST.Subtask
    , _due         :: Maybe Due
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
    description .~
    if null (strip text)
        then Nothing
        else Just text

maybeAppend :: Text -> Maybe Text -> Maybe Text
maybeAppend text (Just current) = Just (concat [current, "\n", text])
maybeAppend text Nothing        = Just text

appendDescription :: Text -> Update
appendDescription text =
    if null (strip text)
        then id
        else description %~ maybeAppend text

setDue :: TZ -> UTCTime -> Text -> Update
setDue tz now date task =
    if null date
        then task & due .~ Nothing
        else case inputToTime tz now date of
                 Just deadline -> task & due ?~ deadline
                 Nothing       -> task

clearDue :: Update
clearDue task = task & due .~ Nothing

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
    check (task ^. name) || maybe False check (task ^. description) || not (null sts)
  where
    check = isInfixOf (toLower text) . toLower
    sts = filter check $ (^. ST.name) <$> (task ^. subtasks)

isBlank :: Task -> Bool
isBlank task =
    null (task ^. name) &&
    isNothing (task ^. description) && null (task ^. subtasks) && isNothing (task ^. due)

duplicate :: Task -> Task
duplicate (Task n d st du) = Task n d (ST.duplicate <$> st) du
