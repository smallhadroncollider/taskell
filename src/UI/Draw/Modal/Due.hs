{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Modal.Due
    ( due
    ) where

import ClassyPrelude

import Brick
import Control.Lens  ((^.))
import Data.Sequence (mapWithIndex)

import qualified Data.Taskell.Lists as L (due)
import           Events.State.Types (lists)
import           UI.Draw.Task       (renderTask)
import           UI.Draw.Types      (DrawState (..), ReaderDrawState)
import           UI.Types           (ResourceName (RNDue))

due :: ReaderDrawState (Text, Widget ResourceName)
due = do
    tasks <- L.due . (^. lists) <$> asks dsState
    widgets <- sequence $ renderTask RNDue (-1) `mapWithIndex` tasks
    pure ("Due Tasks", vBox $ toList widgets)
