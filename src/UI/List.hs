module UI.List where

import Data.Sequence (mapWithIndex) 
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Graphics.Vty

import UI.Task (present)
import Data.Taskell.Task (Tasks)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrNoItems :: Attr
attrNoItems = defAttr `withStyle` dim 

title :: String -> Image
title t = string attrTitle t

noItems :: Image
noItems = string attrNoItems "No items"

tasksToImage :: Seq Image -> Image
tasksToImage = vertCat . toList 

mapTasks :: Tasks -> Image
mapTasks = tasksToImage . mapWithIndex present

list :: String -> Tasks -> Image
list name tasks = (title name) <-> items
    where items = if length tasks /= 0 then mapTasks tasks else noItems 
