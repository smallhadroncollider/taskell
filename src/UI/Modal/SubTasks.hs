{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.SubTasks (
    st
) where

import ClassyPrelude

import Data.Sequence (mapWithIndex)

import Brick

import Data.Taskell.Task (Task, SubTask, description, subTasks, name, complete, summary)
import Events.State (State, getCurrentTask)
import Events.State.Modal.SubTasks (getCurrentSubTask, getField)
import UI.Field (Field, textField, widgetFromMaybe)
import UI.Theme (taskCurrentAttr, disabledAttr)
import UI.Types (ResourceName(..))

renderSubTask :: Maybe Field -> Int -> Int -> SubTask -> Widget ResourceName
renderSubTask f current i subtask = padBottom (Pad 1) final

    where cur = i == current
          prefix = if complete subtask then "[x] " else "[ ] "
          text = prefix ++ name subtask
          widget = textField text
          widget' = widgetFromMaybe widget f
          final | cur = visible $ withAttr taskCurrentAttr widget'
                | complete subtask = withAttr disabledAttr widget
                | otherwise = widget

renderSummary :: Task -> Widget ResourceName
renderSummary task = padTop (Pad 1) $ padBottom (Pad 2) w
    where w = case summary task of
            Just s -> txt s
            Nothing -> txt "No description"

st :: State -> (Text, Widget ResourceName)
st state = fromMaybe ("Error", txt "Oops") $ do
    task <- getCurrentTask state
    i <- getCurrentSubTask state
    let f = getField state

    let sts = subTasks task
        w | null sts = withAttr disabledAttr $ txt "No sub-tasks"
          | otherwise = vBox . toList $ renderSubTask f i `mapWithIndex` sts

    return (description task, renderSummary task <=> w)
