module UI.Main where

import Graphics.Vty
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, mapWithIndex, (><))

import Flow.State (State, Pointer, Size, lists, current, size)

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

rCurList :: Size -> Int -> ListUI -> Maybe (Image, Int, Int)
rCurList (w, h) i (t, ts) = do
    (a, c, b) <- splitOn i ts
    let title = img attrCurrentTitle t
    let r = vCat . fmap (marginTop . task)
    let pre = r a
    let cur = marginTop (currentTask c)
    let y = imageHeight pre + imageHeight cur
    let x = if not (null c) then length (last c) else 0
    let o = calcOffset (h `div` 2) y
    let img = margin $ title <-> pre <-> cur <-> r b
    return (translateY o img, x, y + o)

cur :: Pointer -> Size -> Seq ListUI -> Maybe (Image, Int, Int, Int)
cur (l, i) s ls = do
    (a, c, b) <- splitOn l ls
    let r = hCat . fmap rList
    let pre = r a
    (cur, x, y) <- rCurList s i c
    let img = pre <|> cur <|> r b
    return (img, imageWidth pre, x, y)

calcOffset :: Int -> Int -> Int
calcOffset pivot n = if n > pivot then pivot - n else 0

offset :: Size -> Int -> Int
offset (w, _) = calcOffset (w `div` 3)

-- draws the screen
pic' :: State -> Maybe Picture
pic' s = do
    let ls = present <$> lists s
    let sz = size s
    (img, w, x, y) <- cur (current s) sz ls
    let o = offset sz w
    return $ Picture (Cursor (w + x + o + padding) (y + 1)) [translateX o $ marginTop img] ClearBackground

pic :: State -> Picture
pic s = fromMaybe (picForImage $ string attrError "Something's gone wrong...") (pic' s)

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
