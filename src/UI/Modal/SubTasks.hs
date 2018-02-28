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
import Data.Maybe (fromMaybe)

import UI.Types (ResourceName(..))
import UI.Theme (taskCurrentAttr, disabledAttr)

import UI.Field(Field, field, textField)

renderSubTask :: Maybe Field -> Int -> Int -> SubTask -> Widget ResourceName
renderSubTask f current i subtask = padBottom (Pad 1) final

    where cur = i == current
          postfix = if complete subtask then " ✓" else ""
          text = name subtask `T.append` postfix
          widget = textField text
          widget' = case f of
              Just f' -> field (RNModalItem i) f'
              Nothing -> widget
          final | cur = visible $ withAttr taskCurrentAttr widget'
                | complete subtask = withAttr disabledAttr widget
                | otherwise = widget

st :: State -> (Text, Widget ResourceName)
st state = fromMaybe ("Error", txt "Oops") $ do
    task <- getCurrentTask state
    index <- getCurrentSubTask state
    let f = getField state

    let sts = subTasks task
        w | null sts = withAttr disabledAttr $ txt "No sub-tasks"
          | otherwise = vBox . toList $ renderSubTask f index `mapWithIndex` sts

    return (description task, w)
