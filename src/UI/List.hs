module UI.List where

import Data.Sequence (mapWithIndex) 
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Graphics.Vty

import UI.Task (present)
import Data.Taskell.Task (Tasks)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

title :: String -> Image
title t = string attrTitle t

tasksToImage :: Seq Image -> Image
tasksToImage = vertCat . toList 

list :: String -> Tasks -> Image
list name = (<->) (title name) . tasksToImage . mapWithIndex present
