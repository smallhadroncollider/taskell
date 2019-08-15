{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Modal.Due
    ( due
    ) where

import ClassyPrelude

import Brick
import Data.Taskell.Seq ((<#>))

import qualified Data.Taskell.Task as T (Task)
import           UI.Draw.Task      (TaskWidget (..), parts)
import           UI.Draw.Types     (ReaderDrawState)
import           UI.Theme          (taskAttr, taskCurrentAttr)
import           UI.Types          (ResourceName (RNDue))

renderTask :: Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask current position task = do
    (TaskWidget text date _ _) <- parts task
    let selected = current == position
    let attr =
            if selected
                then taskCurrentAttr
                else taskAttr
    pure .
        (if selected
             then visible
             else id) .
        cached (RNDue position) . padBottom (Pad 1) . withAttr attr $
        vBox [date, text]

due :: Seq T.Task -> Int -> ReaderDrawState (Text, Widget ResourceName)
due tasks selected = do
    widgets <- sequence $ renderTask selected <#> tasks
    pure ("Due Tasks", vBox $ toList widgets)
