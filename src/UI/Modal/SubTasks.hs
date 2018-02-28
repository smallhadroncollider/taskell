{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.SubTasks (
    st
) where

import Events.State (State, getCurrentTask)
import Events.State.Modal.SubTasks (getCurrentSubTask, getField)
import Brick
import Data.Taskell.Task (SubTask, description, subTasks, name, complete)
import Data.Text as T (Text, append)
import Data.Foldable (toList)
import Data.Sequence (mapWithIndex)
import qualified Data.Maybe as Maybe (fromMaybe)

import UI.Types (ResourceName(..))
import UI.Theme (taskCurrentAttr, disabledAttr)

import UI.Field(Field, textField, fromMaybe)

renderSubTask :: Maybe Field -> Int -> Int -> SubTask -> Widget ResourceName
renderSubTask f current i subtask = padBottom (Pad 1) final

    where cur = i == current
          postfix = if complete subtask then " ✓" else ""
          text = name subtask `T.append` postfix
          widget = textField text
          widget' = fromMaybe widget f
          final | cur = visible $ withAttr taskCurrentAttr widget'
                | complete subtask = withAttr disabledAttr widget
                | otherwise = widget

st :: State -> (Text, Widget ResourceName)
st state = Maybe.fromMaybe ("Error", txt "Oops") $ do
    task <- getCurrentTask state
    index <- getCurrentSubTask state
    let f = getField state

    let sts = subTasks task
        w | null sts = withAttr disabledAttr $ txt "No sub-tasks"
          | otherwise = vBox . toList $ renderSubTask f index `mapWithIndex` sts

    return (description task, w)
