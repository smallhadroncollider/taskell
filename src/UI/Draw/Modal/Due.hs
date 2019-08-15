{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Modal.Due
    ( due
    ) where

import ClassyPrelude

import Brick
import Data.Taskell.Seq ((<#>))

import qualified Data.Taskell.Task as T (Task)
import           Types             (Pointer)
import           UI.Draw.Task      (TaskWidget (..), parts)
import           UI.Draw.Types     (DSWidget, ModalWidget)
import           UI.Theme          (taskAttr, taskCurrentAttr)
import           UI.Types          (ResourceName (RNDue))

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
    widgets <- sequence $ renderTask selected <#> (snd <$> tasks)
    pure ("Due Tasks", vBox $ toList widgets)
