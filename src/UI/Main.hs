module UI.Main where

import Graphics.Vty hiding (showCursor)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, mapWithIndex, (><))

import Flow.State (State, Pointer, Size, lists, current, size, newList, showCursor)

import UI.Styles

import Config (width, padding)
import Data.Taskell.String (wrap)
import Data.Taskell.Seq (splitOn)
import Data.Taskell.Task (description)
import Data.Taskell.List (List, tasks, title)

type TaskUI = [String]
type ListUI = (TaskUI, Seq TaskUI)

present :: List -> ListUI
present l = (wrap width (title l), wrap width . description <$> tasks l)

rList :: ListUI -> Image
rList (t, ts) = margin $ img attrTitle t <-> (vCat . fmap (marginTop . task)) ts

justTitle :: TaskUI -> Image
justTitle = img attrCurrentTitle

len :: TaskUI -> Int
len = sum . fmap length

curList :: Size -> Int -> TaskUI -> (Seq TaskUI, TaskUI, Seq TaskUI) -> (Image, Int, Int)
curList (w, h) i t (a, c, b) = (translateY o img, x, y + o)
    where title = justTitle t 
          r = vCat . fmap (marginTop . task)
          pre = r a
          cur = marginTop (currentTask c)
          y = imageHeight pre + imageHeight cur
          x = if not (null c) then length (last c) else 0
          o = calcOffset (h `div` 2) y
          img = margin $ title <-> pre <-> cur <-> r b

rCurList :: Size -> Int -> ListUI -> (Image, Int, Int)
rCurList s i (t, ts) = case splitOn i ts of
    Just l -> curList s i t l
    Nothing -> (margin (justTitle t), len t, 0)

cur' :: Pointer -> Size -> Seq ListUI -> Maybe (Image, Int, Int, Int)
cur' (l, i) s ls = do
    (a, c, b) <- splitOn l ls
    let r = hCat . fmap rList
    let pre = r a
    let (cur, x, y) = rCurList s i c
    let img = pre <|> cur <|> r b
    return (img, imageWidth pre, x, y)

cur :: Pointer -> Size -> Seq ListUI -> (Image, Int, Int, Int)
cur p s ls = fromMaybe (string attrNormal "No lists", 0, 0, 0) c
    where c = cur' p s ls

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
          (img, w, x, y) = cur (current s') sz ls
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
