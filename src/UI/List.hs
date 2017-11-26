module UI.List where

import Data.Sequence (Seq, mapWithIndex) 
import Data.Foldable (toList)
import Graphics.Vty

import Data.Taskell.Task (Task)
import Data.Taskell.List
import Flow.State (State, current, size)
import Config (width)

import UI.Task (present)
import UI.Styles

titleImage :: Bool -> String -> Image
titleImage current s = string style ("  " ++ s)
    where style = if current then attrCurrent else attrTitle

noItems :: Image
noItems = resizeWidth width $ string attrNoItems "  No items"

tasksToImage :: Seq Image -> Image
tasksToImage = vertCat . toList 

-- passing current and index feels inelegant...
mapTasks :: Bool -> Int -> Seq Task -> Image
mapTasks current index = tasksToImage . mapWithIndex (present current index)

list :: State -> Int -> Int -> List -> Image
list s titleOffset n (List title ts) = translateY offset $ t <-> tasks
    where (l, i) = current s
          (w, h) = size s
          mid = (h `div` 2) - titleOffset
          current' = l == n
          offset = if current' && i > mid then mid - i else 0
          t = titleImage current' (show (n + 1) ++ ". " ++ title)
          tasks = if not (null ts) then mapTasks current' i ts else noItems 
          
