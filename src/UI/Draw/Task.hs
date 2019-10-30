{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Task
    ( TaskWidget(..)
    , renderTask
    , parts
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick

import           Data.Taskell.Date  (dayToText, deadline)
import qualified Data.Taskell.Task  as T (Task, contains, countCompleteSubtasks, countSubtasks,
                                          description, due, hasSubtasks, name)
import           Events.State.Types (current, mode, searchTerm, time)
import           IO.Config.Layout   (descriptionIndicator)
import           Types              (ListIndex (..), TaskIndex (..))
import           UI.Draw.Field      (getText, textField, widgetFromMaybe)
import           UI.Draw.Mode
import           UI.Draw.Types      (DSWidget, DrawState (..), ReaderDrawState, TWidget)
import           UI.Theme
import           UI.Types           (ResourceName)

data TaskWidget = TaskWidget
    { textW     :: TWidget
    , dateW     :: TWidget
    , summaryW  :: TWidget
    , subtasksW :: TWidget
    }

-- | Takes a task's 'due' property and renders a date with appropriate styling (e.g. red if overdue)
renderDate :: T.Task -> DSWidget
renderDate task = do
    today <- utctDay . (^. time) <$> asks dsState
    pure . fromMaybe emptyWidget $
        (\day -> withAttr (dlToAttr $ deadline today day) (txt $ dayToText today day)) <$>
        task ^. T.due

-- | Renders the appropriate completed sub task count e.g. "[2/3]"
renderSubtaskCount :: T.Task -> DSWidget
renderSubtaskCount task =
    pure . fromMaybe emptyWidget $ bool Nothing (Just indicator) (T.hasSubtasks task)
  where
    complete = tshow $ T.countCompleteSubtasks task
    total = tshow $ T.countSubtasks task
    indicator = txt $ concat ["[", complete, "/", total, "]"]

-- | Renders the description indicator
renderDescIndicator :: T.Task -> DSWidget
renderDescIndicator task = do
    indicator <- descriptionIndicator <$> asks dsLayout
    pure . fromMaybe emptyWidget $ const (txt indicator) <$> task ^. T.description -- show the description indicator if one is set

-- | Renders the task text
renderText :: T.Task -> DSWidget
renderText task = pure $ textField (task ^. T.name)

-- | Renders the appropriate indicators: description, sub task count, and due date
indicators :: T.Task -> DSWidget
indicators task = do
    widgets <- sequence (($ task) <$> [renderDescIndicator, renderSubtaskCount, renderDate])
    pure . hBox $ padRight (Pad 1) <$> widgets

-- | The individual parts of a task widget
parts :: T.Task -> ReaderDrawState TaskWidget
parts task =
    TaskWidget <$> renderText task <*> renderDate task <*> renderDescIndicator task <*>
    renderSubtaskCount task

-- | Renders an individual task
renderTask' :: (Int -> ResourceName) -> Int -> Int -> T.Task -> DSWidget
renderTask' rn listIndex taskIndex task = do
    eTitle <- editingTitle . (^. mode) <$> asks dsState -- is the title being edited? (for visibility)
    selected <- (== (ListIndex listIndex, TaskIndex taskIndex)) . (^. current) <$> asks dsState -- is the current task selected?
    taskField <- getField . (^. mode) <$> asks dsState -- get the field, if it's being edited
    after <- indicators task -- get the indicators widget
    widget <- renderText task
    let name = rn taskIndex
        widget' = widgetFromMaybe widget taskField
    pure $
        cached name .
        (if selected && not eTitle
             then visible
             else id) .
        padBottom (Pad 1) .
        (<=> withAttr disabledAttr after) .
        withAttr
            (if selected
                 then taskCurrentAttr
                 else taskAttr) $
        if selected && not eTitle
            then widget'
            else widget

renderTask :: (Int -> ResourceName) -> Int -> Int -> T.Task -> DSWidget
renderTask rn listIndex taskIndex task = do
    searchT <- (getText <$>) . (^. searchTerm) <$> asks dsState
    let taskWidget = renderTask' rn listIndex taskIndex task
    case searchT of
        Nothing -> taskWidget
        Just term ->
            if T.contains term task
                then taskWidget
                else pure emptyWidget
