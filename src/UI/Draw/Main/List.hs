{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module UI.Draw.Main.List
    ( renderList
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Char     (chr, ord)
import Data.Sequence (mapWithIndex)

import Brick

import Data.Taskell.List  (List, tasks, title)
import Events.State.Types (current, mode)
import IO.Config.Layout   (columnPadding, columnWidth)
import Types              (ListIndex (ListIndex), TaskIndex (TaskIndex))
import UI.Draw.Field      (textField, widgetFromMaybe)
import UI.Draw.Mode
import UI.Draw.Task       (renderTask)
import UI.Draw.Types      (DSWidget, DrawState (..), ReaderDrawState)
import UI.Theme
import UI.Types           (ResourceName (..))

-- | Gets the relevant column prefix - number in normal mode, letter in moveTo
columnPrefix :: Int -> Int -> ReaderDrawState Text
columnPrefix selectedList i = do
    m <- (^. mode) <$> asks dsState
    if moveTo m
        then do
            let col = chr (i + ord 'a')
            pure $
                if i /= selectedList && i >= 0 && i <= 26
                    then singleton col <> ". "
                    else ""
        else do
            let col = i + 1
            pure $
                if col >= 1 && col <= 9
                    then tshow col <> ". "
                    else ""

-- | Renders the title for a list
renderTitle :: Int -> List -> DSWidget
renderTitle listIndex list = do
    (ListIndex selectedList, TaskIndex selectedTask) <- (^. current) <$> asks dsState
    editing <- (selectedList == listIndex &&) . editingTitle . (^. mode) <$> asks dsState
    titleField <- getField . (^. mode) <$> asks dsState
    col <- txt <$> columnPrefix selectedList listIndex
    let text = list ^. title
        attr =
            if selectedList == listIndex
                then titleCurrentAttr
                else titleAttr
        widget = textField text
        widget' = widgetFromMaybe widget titleField
        title' =
            padBottom (Pad 1) . withAttr attr . (col <+>) $
            if editing
                then widget'
                else widget
    pure $
        if editing || selectedList /= listIndex || selectedTask == 0
            then visible title'
            else title'

-- | Renders a list
renderList :: Int -> List -> DSWidget
renderList listIndex list = do
    layout <- dsLayout <$> ask
    eTitle <- editingTitle . (^. mode) <$> asks dsState
    titleWidget <- renderTitle listIndex list
    (ListIndex currentList, _) <- (^. current) <$> asks dsState
    taskWidgets <-
        sequence $
        renderTask (RNTask . (ListIndex listIndex, ) . TaskIndex) listIndex `mapWithIndex`
        (list ^. tasks)
    let widget =
            (if not eTitle
                 then cached (RNList listIndex)
                 else id) .
            padLeftRight (columnPadding layout) .
            hLimit (columnWidth layout) .
            viewport (RNList listIndex) Vertical . vBox . (titleWidget :) $
            toList taskWidgets
    pure $
        if currentList == listIndex
            then visible widget
            else widget
