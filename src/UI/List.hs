module UI.List where

import Data.Sequence (Seq, mapWithIndex) 
import Data.Foldable (toList)
import Graphics.Vty

import UI.Task (present)
import Data.Taskell.Task (Task)
import Data.Taskell.List
import Config (width)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` blue

attrNoItems :: Attr
attrNoItems = defAttr `withStyle` dim 

titleImage :: Bool -> String -> Image
titleImage current = string style
    where style = if current then attrCurrent else attrTitle

noItems :: Image
noItems = resizeWidth width $ string attrNoItems "No items"

tasksToImage :: Seq Image -> Image
tasksToImage = vertCat . toList 

-- passing current and index feels inelegant...
mapTasks :: Bool -> Int -> Seq Task -> Image
mapTasks current index = tasksToImage . mapWithIndex (present current index)

list :: (Int, Int) -> Int -> List -> Image
list (l, i) n (List title ts) = t <-> tasks
    where current' = l == n
          t = titleImage current' (show (n + 1) ++ ". " ++ title)
          tasks = if not (null ts) then mapTasks current' i ts else noItems 
          
