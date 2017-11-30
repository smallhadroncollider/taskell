module UI.Main where

import Graphics.Vty hiding (showCursor)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, mapWithIndex, (><))

import Flow.State (State, Pointer, Size, lists, current, size, newList, showCursor)

import UI.Styles

import Config (width, padding)
import Data.Taskell.String (wrap)
import Data.Taskell.Seq (Split, splitOn)
import Data.Taskell.Task (description)
import Data.Taskell.List (List, tasks, title)

type TaskUI = [String]
type ListUI = (TaskUI, Seq TaskUI)

columnNumber :: Int -> String -> String
columnNumber i s = if col >= 1 && col <= 9 then show col ++ ". " ++ s else s
    where col = i + 1

present :: Int -> List -> ListUI
present i l = (wrap width (columnNumber i $ title l), wrap width . description <$> tasks l)

currentTitleImage :: TaskUI -> Image
currentTitleImage = img attrCurrentTitle

taskLength:: TaskUI -> Int
taskLength = sum . fmap length

tasksImage :: Seq TaskUI -> Image
tasksImage = vCat . fmap (marginTop . task)

renderCurrentList' :: Size -> TaskUI -> Split TaskUI -> (Image, Int, Int)
renderCurrentList' (_, height) task (before, current, after) = (translateY yOffset img, x, y + yOffset)
    where title = currentTitleImage task
          [before', after'] = tasksImage <$> [before, after] 
          current' = marginTop (currentTask current)
          y = sum $ imageHeight <$> [before', current']
          x = if not (null current) then length (last current) else 0
          yOffset = calcOffset (height `div` 2) y
          img = margin $ vertCat [title, before', current', after']

renderCurrentList :: Size -> Int -> ListUI -> (Image, Int, Int)
renderCurrentList size index (title, tasks) = case splitOn index tasks of
    Just list -> renderCurrentList' size title list
    Nothing -> (margin (currentTitleImage title), taskLength title, 0)

listImage :: ListUI -> Image
listImage (title, tasks) = margin $ img attrTitle title <-> tasksImage tasks

listsImage :: Seq ListUI -> Image
listsImage = hCat . fmap listImage

renderLists' :: Pointer -> Size -> Seq ListUI -> Maybe (Image, Int, Int, Int)
renderLists' (list, index) size lists = do
    (before, current, after) <- splitOn list lists
    let [before', after'] = listsImage <$> [before, after]
    let (current', x, y) = renderCurrentList size index current
    let img = horizCat [before', current', after']
    return (img, imageWidth before', x, y)

renderLists :: Pointer -> Size -> Seq ListUI -> (Image, Int, Int, Int)
renderLists p s ls = fromMaybe (string attrNormal "No lists", 0, 0, 0) c
    where c = renderLists' p s ls

calcOffset :: Int -> Int -> Int
calcOffset pivot n = if n > pivot then pivot - n else 0

-- draws the screen
pic :: State -> Picture
pic state = Picture cursor [translateX o $ marginTop img] ClearBackground
    where state' = newList state
          sz = size state'
          ls = mapWithIndex present $ lists state' 
          (img, w, x, y) = renderLists (current state') sz ls
          o = calcOffset (fst sz `div` 3) w
          cursor = if showCursor state' then Cursor (w + x + o + padding) (y + 1) else NoCursor

-- styling
task :: TaskUI -> Image
task = img attrNormal 

currentTask :: TaskUI -> Image
currentTask = img attrCurrent

-- vty helpers
img :: Attr -> TaskUI -> Image
img a s = vertCat $ string a <$> s

hCat :: Seq Image -> Image
hCat = foldl (<|>) emptyImage

vCat :: Seq Image -> Image
vCat = foldl (<->) emptyImage
