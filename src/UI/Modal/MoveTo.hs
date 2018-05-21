{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.MoveTo (
    moveTo
) where

import ClassyPrelude

import Brick

import Data.Taskell.List (title)
import Events.State (State, lists, getCurrentList)
import UI.Field (textField)
import UI.Theme (taskCurrentAttr)
import UI.Types (ResourceName)

moveTo :: State -> (Text, Widget ResourceName)
moveTo state = ("Move To:", widget)
    where skip = getCurrentList state
          ls = toList $ lists state
          titles = textField . title <$> ls

          letter a = padRight (Pad 1) . hBox $ [txt "[", withAttr taskCurrentAttr $ txt (singleton a), txt "]"]
          letters = letter <$> ['a'..]

          remove i l = take i l ++ drop (i + 1) l
          output (l, t) = l <+> t
          widget = vBox $ output <$> remove skip (zip letters titles)
