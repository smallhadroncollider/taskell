{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.Detail (
    detail
) where

import ClassyPrelude

import Data.Sequence (mapWithIndex)

import Brick

import Data.Taskell.Task (Task, SubTask, description, subTasks, name, complete, summary)
import Events.State (State, getCurrentTask)
import Events.State.Types (DetailItem(..))
import Events.State.Modal.Detail (getCurrentItem, getField)
import UI.Field (Field, textField, widgetFromMaybe)
import UI.Theme (taskCurrentAttr, disabledAttr, titleCurrentAttr)
import UI.Types (ResourceName(..))

renderSubTask :: Maybe Field -> DetailItem -> Int -> SubTask -> Widget ResourceName
renderSubTask f current i subtask = padBottom (Pad 1) $ prefix <+> final

    where cur = case current of
              DetailItem c -> i == c
              _ -> False
          done = complete subtask
          attr = withAttr (if cur then taskCurrentAttr else titleCurrentAttr)
          prefix = attr . txt $ if done then "[x] " else "[ ] "
          widget = textField (name subtask)
          final | cur = visible . attr $ widgetFromMaybe widget f
                | not done = attr widget
                | otherwise = widget

renderSummary :: Maybe Field -> DetailItem -> Task -> Widget ResourceName
renderSummary f i task = padTop (Pad 1) $ padBottom (Pad 2) w'
    where w = textField $ fromMaybe "No description" (summary task)
          w' = case i of
            DetailDescription -> visible $ widgetFromMaybe w f
            _ -> w

detail :: State -> (Text, Widget ResourceName)
detail state = fromMaybe ("Error", txt "Oops") $ do
    task <- getCurrentTask state
    i <- getCurrentItem state
    let f = getField state

    let sts = subTasks task
        w | null sts = withAttr disabledAttr $ txt "No sub-tasks"
          | otherwise = vBox . toList $ renderSubTask f i `mapWithIndex` sts

    return (description task, renderSummary f i task <=> w)
