module UI.List where

import Data.Sequence (mapWithIndex) 
import Data.Foldable (toList)
import Graphics.Vty

import UI.Task (present)
import Data.Taskell.Task (Tasks)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

title :: String -> Image
title t = string attrTitle t

list :: (Tasks -> Tasks) -> String -> Tasks -> Image
list fn name tasks = (title name) <-> items
    where items = vertCat $ toList $ mapWithIndex present $ fn $ tasks
