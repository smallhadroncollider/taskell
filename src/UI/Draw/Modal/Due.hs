{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Modal.Due
    ( due
    ) where

import ClassyPrelude

import Brick
import Control.Lens ((^.))

import Data.Taskell.Seq ((<#>))

import qualified Data.Taskell.Task as T (Task, due)
import           Types             (Pointer)
import           UI.Draw.Task      (TaskWidget (..), parts)
import           UI.Draw.Types     (ModalWidget, ReaderDrawState, TWidget)
import           UI.Theme          (taskAttr, taskCurrentAttr)
import           UI.Types          (ResourceName (RNDue))

type DueItem = (Maybe Day, TWidget, TWidget)

renderTask :: Int -> Int -> T.Task -> ReaderDrawState DueItem
renderTask current position task = do
    (TaskWidget text date _ _) <- parts task
    let selected = current == position
    let attr =
            if selected
                then taskCurrentAttr
                else taskAttr
    let shw =
            if selected
                then visible
                else id
    let tWidget = shw . cached (RNDue position) . padBottom (Pad 1) . withAttr attr $ text
    pure (task ^. T.due, date, tWidget)

renderList :: [(Maybe Day, TWidget)] -> DueItem -> [(Maybe Day, TWidget)]
renderList acc (day, date, text) =
    case lastMay lst of
        Just (prevDay, widget) ->
            if prevDay == day
                then rst <> [(day, widget <=> text)]
                else acc <> new
        Nothing -> new
  where
    (rst, lst) = splitAt (length acc - 1) acc
    new = [(day, padTop (Pad 1) date <=> text)]

due :: Seq (Pointer, T.Task) -> Int -> ModalWidget
due tasks selected = do
    widgets <- sequence $ renderTask selected <#> (snd <$> tasks)
    let list = foldl' renderList empty widgets
    pure ("Due Tasks", vBox $ snd <$> list)
