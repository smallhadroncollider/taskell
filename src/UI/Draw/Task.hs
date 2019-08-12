{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Task
    ( renderTask
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick

import           Data.Taskell.Date  (dayToText, deadline)
import qualified Data.Taskell.Task  as T (Task, contains, countCompleteSubtasks, countSubtasks,
                                          description, due, hasSubtasks, name)
import           Events.State.Types (current, mode, searchTerm)
import           IO.Config.Layout   (descriptionIndicator)
import           UI.Draw.Field      (getText, textField, widgetFromMaybe)
import           UI.Draw.Mode
import           UI.Draw.Types      (DrawState (..), ReaderDrawState)
import           UI.Theme
import           UI.Types           (ResourceName)

-- | Takes a task's 'due' property and renders a date with appropriate styling (e.g. red if overdue)
renderDate :: T.Task -> ReaderDrawState (Maybe (Widget ResourceName))
renderDate task = do
    today <- asks dsToday -- get the value of `today` from DrawState
    pure $
        (\day -> withAttr (dlToAttr $ deadline today day) (txt $ dayToText today day)) <$>
        task ^. T.due

-- | Renders the appropriate completed sub task count e.g. "[2/3]"
renderSubtaskCount :: T.Task -> ReaderDrawState (Maybe (Widget ResourceName))
renderSubtaskCount task = pure $ bool Nothing (Just indicator) (T.hasSubtasks task)
  where
    complete = tshow $ T.countCompleteSubtasks task
    total = tshow $ T.countSubtasks task
    indicator = txt $ concat ["[", complete, "/", total, "]"]

-- | Renders the description indicator
renderDescIndicator :: T.Task -> ReaderDrawState (Maybe (Widget ResourceName))
renderDescIndicator task = do
    indicator <- descriptionIndicator <$> asks dsLayout
    pure $ const (txt indicator) <$> task ^. T.description -- show the description indicator if one is set

-- | Renders the appropriate indicators: description, sub task count, and due date
indicators :: T.Task -> ReaderDrawState (Widget ResourceName)
indicators task = do
    widgets <-
        catMaybes <$> sequence [renderDescIndicator task, renderSubtaskCount task, renderDate task]
    pure . hBox $ padRight (Pad 1) <$> widgets

-- | Renders an individual task
renderTask' ::
       (Int -> ResourceName) -> Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask' rn listIndex taskIndex task = do
    eTitle <- editingTitle . (^. mode) <$> asks dsState -- is the title being edited? (for visibility)
    selected <- (== (listIndex, taskIndex)) . (^. current) <$> asks dsState -- is the current task selected?
    taskField <- getField . (^. mode) <$> asks dsState -- get the field, if it's being edited
    after <- indicators task -- get the indicators widget
    let text = task ^. T.name
        name = rn taskIndex
        widget = textField text
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

renderTask :: (Int -> ResourceName) -> Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask rn listIndex taskIndex task = do
    searchT <- (getText <$>) . (^. searchTerm) <$> asks dsState
    let taskWidget = renderTask' rn listIndex taskIndex task
    case searchT of
        Nothing -> taskWidget
        Just term ->
            if T.contains term task
                then taskWidget
                else pure emptyWidget
