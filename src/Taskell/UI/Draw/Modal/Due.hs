{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.UI.Draw.Modal.Due
    ( due
    ) where

import ClassyPrelude

import Brick
import Taskell.Data.Seq ((<#>))

import qualified Taskell.Data.Task     as T (Task)
import           Taskell.Types         (Pointer)
import           Taskell.UI.Draw.Task  (TaskWidget (..), parts)
import           Taskell.UI.Draw.Types (DSWidget, ModalWidget)
import           Taskell.UI.Theme      (taskAttr, taskCurrentAttr)
import           Taskell.UI.Types      (ResourceName (RNDue))

renderTask :: Int -> Int -> T.Task -> DSWidget
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
    pure . shw . cached (RNDue position) . padBottom (Pad 1) . withAttr attr $ vBox [date, text]

due :: Seq (Pointer, T.Task) -> Int -> ModalWidget
due tasks selected = do
    let items = snd <$> tasks
    widgets <- sequence $ renderTask selected <#> items
    pure
        ( "Due Tasks"
        , if null items
              then txt "No due tasks"
              else vBox $ toList widgets)
