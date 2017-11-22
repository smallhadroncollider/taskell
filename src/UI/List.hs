module UI.List where

import Data.Sequence (Seq, mapWithIndex) 
import Data.Foldable (toList)
import Graphics.Vty

import UI.Task (present)
import Data.Taskell.Task (Task)
import Data.Taskell.Tasks

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` blue

attrNoItems :: Attr
attrNoItems = defAttr `withStyle` dim 

title :: Bool -> String -> Image
title current = string style
    where style = if current then attrCurrent else attrTitle

noItems :: Image
noItems = string attrNoItems "No items"

tasksToImage :: Seq Image -> Image
tasksToImage = vertCat . toList 

-- passing current and index feels inelegant...
mapTasks :: Bool -> Int -> Seq Task -> Image
mapTasks current index = tasksToImage . mapWithIndex (present current index)

list :: Tasks -> Bool -> Int -> Image
list (Tasks name tasks) current index = title current name <-> items
    where items = if not (null tasks) then mapTasks current index tasks else noItems 
