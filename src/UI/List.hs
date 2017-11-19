module UI.List where

import Data.Sequence (mapWithIndex) 
import Data.Foldable (toList)
import Graphics.Vty

import UI.Task (present)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

title t = string attrTitle t

list fn name tasks = (title name) <-> items
    where items = vertCat $ toList $ mapWithIndex present $ fn $ tasks
