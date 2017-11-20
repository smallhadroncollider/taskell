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

-- passing current and index feels inelegant...
mapTasks :: Bool -> Int -> Tasks -> Image
mapTasks current index = tasksToImage . mapWithIndex (present current index)

list :: String -> Bool -> Int -> Tasks -> Image
list name current index tasks = (title name) <-> items
    where items = if length tasks /= 0 then (mapTasks current index tasks) else noItems 
