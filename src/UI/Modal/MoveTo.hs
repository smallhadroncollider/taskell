{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.MoveTo (
    moveTo
) where

import Events.State (State, lists)
import Brick
import Data.Taskell.List (title)
import Data.Text as T (Text, concat, singleton)
import Data.Taskell.Text (wrap)
import Data.Foldable (toList)

import UI.Types (ResourceName)
import UI.Internal (box)

moveTo :: State -> Int -> (Text, Widget ResourceName)
moveTo state width = ("Move To:", widget)
    where ls = toList $ lists state
          text (a, list) = T.concat ["[", singleton a, "] ", title list]
          widget = vBox $ box 0 . wrap width . text <$> zip ['a'..] ls
