{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.MoveTo (
    moveTo
) where

import Events.State (State, lists, getCurrentList)
import Brick
import Data.Taskell.List (title)
import Data.Text as T (Text, concat, singleton)
import Data.Sequence (deleteAt)
import Data.Foldable (toList)

import UI.Field (textField)
import UI.Types (ResourceName)

moveTo :: State -> (Text, Widget ResourceName)
moveTo state = ("Move To:", widget)
    where skip = getCurrentList state
          ls = toList . deleteAt skip $ lists state
          titles = textField . title <$> ls

          letter a = padRight (Pad 1) . txt $ T.concat ["[", singleton a, "]"]
          letters = letter <$> ['a'..]

          output (l, t) = l <+> t
          widget = vBox $ output <$> zip letters titles
