{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Task
    ( renderTask
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick

import           Data.Taskell.Date  (Day, dayToText, deadline)
import qualified Data.Taskell.Task  as T (Task, contains, countCompleteSubtasks, countSubtasks,
                                          description, due, hasSubtasks, name)
import           Events.State.Types (current, mode, searchTerm)
import           IO.Config.Layout   (descriptionIndicator)
import           UI.Draw.Field      (getText, textField, widgetFromMaybe)
import           UI.Draw.Mode
import           UI.Draw.Types      (DrawState (..), ReaderDrawState)
import           UI.Theme
import           UI.Types           (ListIndex (..), ResourceName (..), TaskIndex (..))

-- | Takes a task's 'due' property and renders a date with appropriate styling (e.g. red if overdue)
renderDate :: Maybe Day -> ReaderDrawState (Maybe (Widget ResourceName))
renderDate dueDay = do
    today <- dsToday <$> ask -- get the value of `today` from DrawState
    let attr = withAttr . dlToAttr . deadline today <$> dueDay -- create a `Maybe (Widget -> Widget)` attribute function
        widget = txt . dayToText today <$> dueDay -- get the formatted due date `Maybe Text`
    pure $ attr <*> widget

-- | Renders the appropriate completed sub task count e.g. "[2/3]"
renderSubtaskCount :: T.Task -> Widget ResourceName
renderSubtaskCount task =
    txt $ concat ["[", tshow $ T.countCompleteSubtasks task, "/", tshow $ T.countSubtasks task, "]"]

-- | Renders the appropriate indicators: description, sub task count, and due date
indicators :: T.Task -> ReaderDrawState (Widget ResourceName)
indicators task = do
    dateWidget <- renderDate (task ^. T.due) -- get the due date widget
    descIndicator <- descriptionIndicator . dsLayout <$> ask
    pure . hBox $
        padRight (Pad 1) <$>
        catMaybes
            [ const (txt descIndicator) <$> task ^. T.description -- show the description indicator if one is set
            , bool Nothing (Just (renderSubtaskCount task)) (T.hasSubtasks task) -- if it has subtasks, render the sub task count
            , dateWidget
            ]

-- | Renders an individual task
renderTask' :: Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask' listIndex taskIndex task = do
    eTitle <- editingTitle . (^. mode) <$> asks dsState -- is the title being edited? (for visibility)
    selected <- (== (listIndex, taskIndex)) . (^. current) <$> asks dsState -- is the current task selected?
    taskField <- getField . (^. mode) <$> asks dsState -- get the field, if it's being edited
    after <- indicators task -- get the indicators widget
    let text = task ^. T.name
        name = RNTask (ListIndex listIndex, TaskIndex taskIndex)
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

renderTask :: Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask listIndex taskIndex task = do
    searchT <- (^. searchTerm) <$> asks dsState
    case searchT of
        Nothing -> renderTask' listIndex taskIndex task
        Just term ->
            if T.contains (getText term) task
                then renderTask' listIndex taskIndex task
                else pure emptyWidget
