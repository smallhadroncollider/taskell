{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.MoveTo (
    moveTo
) where

import Events.State (State, lists)
import Brick
import Data.Taskell.List (title)
import Data.Text as T (Text, concat, singleton)
import Data.Foldable (toList)

import UI.Field (textField)
import UI.Types (ResourceName)

moveTo :: State -> (Text, Widget ResourceName)
moveTo state = ("Move To:", widget)
    where ls = toList $ lists state
          titles = textField . title <$> ls

          letter a = padRight (Pad 1) . txt $ T.concat ["[", singleton a, "]"]
          letters = letter <$> ['a'..]

          output (l, t) = l <+> t
          widget = vBox $ output <$> zip letters titles
