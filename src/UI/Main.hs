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

present :: List -> ListUI
present l = (wrap width (title l), wrap width . description <$> tasks l)

titleImage :: TaskUI -> Image
titleImage = img attrCurrentTitle

taskLength:: TaskUI -> Int
taskLength= sum . fmap length

tasksImage :: Seq TaskUI -> Image
tasksImage = vCat . fmap (marginTop . task)

renderCurrentList' :: Size -> Int -> TaskUI -> Split TaskUI -> (Image, Int, Int)
renderCurrentList' (w, h) i t (before, current, after) = (translateY yOffset img, x, y + yOffset)
    where title = titleImage t 
          [before', after'] = tasksImage <$> [before, after] 
          current' = marginTop (currentTask current)
          y = sum $ imageHeight <$> [before', current']
          x = if not (null current) then length (last current) else 0
          yOffset = calcOffset (h `div` 2) y
          img = margin $ vertCat [title, before', current', after']

renderCurrentList :: Size -> Int -> ListUI -> (Image, Int, Int)
renderCurrentList s i (t, ts) = case splitOn i ts of
    Just l -> renderCurrentList' s i t l
    Nothing -> (margin (titleImage t), taskLength t, 0)

listImage :: ListUI -> Image
listImage (t, ts) = margin $ img attrTitle t <-> tasksImage ts

listsImage :: Seq ListUI -> Image
listsImage = hCat . fmap listImage

renderLists' :: Pointer -> Size -> Seq ListUI -> Maybe (Image, Int, Int, Int)
renderLists' (l, i) s ls = do
    (before, current, after) <- splitOn l ls
    let [before', after'] = listsImage <$> [before, after]
    let (current', x, y) = renderCurrentList s i current
    let img = horizCat [before', current', after']
    return (img, imageWidth before', x, y)

renderLists :: Pointer -> Size -> Seq ListUI -> (Image, Int, Int, Int)
renderLists p s ls = fromMaybe (string attrNormal "No lists", 0, 0, 0) c
    where c = renderLists' p s ls

calcOffset :: Int -> Int -> Int
calcOffset pivot n = if n > pivot then pivot - n else 0

offset :: Size -> Int -> Int
offset (w, _) = calcOffset (w `div` 3)

-- draws the screen
pic :: State -> Picture
pic s = Picture cursor [translateX o $ marginTop img] ClearBackground
    where s' = newList s
          ls = present <$> lists s' 
          sz = size s'
          (img, w, x, y) = renderLists (current s') sz ls
          o = offset sz w
          cursor = if showCursor s then Cursor (w + x + o + padding) (y + 1) else NoCursor

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
