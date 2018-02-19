{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.SubTasks (
    st
) where

import Events.State (State, getCurrentTask)
import Events.State.Modal.SubTasks (getCurrentSubTask)
import Brick
import Data.Taskell.Task (SubTask, description, subTasks, name, complete)
import Data.Text as T (Text, length, append)
import Data.Taskell.Text (wrap)
import Data.Foldable (toList)
import Data.Sequence (mapWithIndex)
import Data.Maybe (fromMaybe)

import UI.Types (ResourceName(..))
import UI.Theme (taskCurrentAttr, disabledAttr)

import UI.Internal (box)

addCursor :: Int -> [Text] -> Widget ResourceName -> Widget ResourceName
addCursor li d = showCursor n (Location (h, v))

    where v = Prelude.length d - 1
          h = T.length $ last d
          n = RNModalItem li

renderSubTask :: Int -> Int -> Int -> SubTask -> Widget ResourceName
renderSubTask width current i subtask

    | i == current = visible $ withAttr taskCurrentAttr widget
    | complete subtask = withAttr disabledAttr widget
    | otherwise = widget

    where postfix = if complete subtask then " ✓" else ""
          text = wrap width $ name subtask `T.append` postfix
          widget = addCursor i text (box 1 text)

st :: State -> Int -> (Text, Widget ResourceName)
st state width = fromMaybe ("Error", txt "Oops") $ do
    task <- getCurrentTask state
    index <- getCurrentSubTask state

    let sts = subTasks task
        w | null sts = withAttr disabledAttr $ txt "No sub-tasks"
          | otherwise = vBox . toList $ renderSubTask width index `mapWithIndex` sts

    return (description task, w)
