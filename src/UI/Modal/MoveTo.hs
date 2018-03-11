{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.MoveTo (
    moveTo
) where

import ClassyPrelude

import Data.Sequence (deleteAt)

import Brick

import Data.Taskell.List (title)
import Events.State (State, lists, getCurrentList)
import UI.Field (textField)
import UI.Theme (taskCurrentAttr)
import UI.Types (ResourceName)

moveTo :: State -> (Text, Widget ResourceName)
moveTo state = ("Move To:", widget)
    where skip = getCurrentList state
          ls = toList . deleteAt skip $ lists state
          titles = textField . title <$> ls

          letter a = padRight (Pad 1) . hBox $ [txt "[", withAttr taskCurrentAttr $ txt (singleton a), txt "]"]
          letters = letter <$> ['a'..]

          output (l, t) = l <+> t
          widget = vBox $ output <$> zip letters titles
