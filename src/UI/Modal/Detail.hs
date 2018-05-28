{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.Detail (
    detail
) where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Sequence (mapWithIndex)

import Brick

import Data.Taskell.Date (Day, dayToOutput, deadline)
import Data.Taskell.Task (Task, description, subtasks, summary, due)
import qualified Data.Taskell.Subtask as ST (Subtask, name, complete)
import Events.State (State, getCurrentTask)
import Events.State.Types (DetailItem(..))
import Events.State.Modal.Detail (getCurrentItem, getField)
import UI.Field (Field, textField, widgetFromMaybe)
import UI.Theme (taskCurrentAttr, disabledAttr, titleCurrentAttr, dlToAttr)
import UI.Types (ResourceName(..))

renderSubtask :: Maybe Field -> DetailItem -> Int -> ST.Subtask -> Widget ResourceName
renderSubtask f current i subtask = padBottom (Pad 1) $ prefix <+> final

    where cur = case current of
              DetailItem c -> i == c
              _ -> False
          done = subtask ^. ST.complete
          attr = withAttr (if cur then taskCurrentAttr else titleCurrentAttr)
          prefix = attr . txt $ if done then "[x] " else "[ ] "
          widget = textField (subtask ^. ST.name)
          final | cur = visible . attr $ widgetFromMaybe widget f
                | not done = attr widget
                | otherwise = widget

renderSummary :: Maybe Field -> DetailItem -> Task -> Widget ResourceName
renderSummary f i task = padTop (Pad 1) $ padBottom (Pad 2) w'
    where w = textField $ fromMaybe "No description" (summary task)
          w' = case i of
            DetailDescription -> visible $ widgetFromMaybe w f
            _ -> w

renderDate :: Day -> Maybe Field -> DetailItem -> Task -> Widget ResourceName
renderDate today field item task = case item of
    DetailDate -> visible $ prefix <+> widgetFromMaybe widget field
    _ -> case day of
        Just d -> prefix <+> withAttr (dlToAttr (deadline today d)) widget
        Nothing -> emptyWidget
    where day = due task
          prefix = txt "Due: "
          widget = textField $ maybe "" dayToOutput day

detail :: State -> Day -> (Text, Widget ResourceName)
detail state today = fromMaybe ("Error", txt "Oops") $ do
    task <- getCurrentTask state
    i <- getCurrentItem state
    let f = getField state

    let sts = subtasks task
        w | null sts = withAttr disabledAttr $ txt "No sub-tasks"
          | otherwise = vBox . toList $ renderSubtask f i `mapWithIndex` sts

    return (description task, renderDate today f i task <=> renderSummary f i task <=> w)
