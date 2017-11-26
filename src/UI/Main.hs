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
rList (t, ts) = margin $ renderTitle False t <-> (vCat . fmap (marginTop . task)) ts

rCurList' :: Size -> Int -> ListUI -> Maybe Image
rCurList' (w, h) i (t, ts) = do
    (a, c, b) <- splitOn i ts
    let title = renderTitle True t
    let r = vCat . fmap (marginTop . task)
    let pre = r a
    let cur = marginTop (currentTask c)
    let o = calcOffset (h `div` 2) (imageHeight pre + imageHeight cur) 
    let img = margin $ title <-> pre <-> cur <-> r b
    return $ translateY o img

rCurList :: Size -> Int -> ListUI -> Image
rCurList s i l = fromMaybe emptyImage (rCurList' s i l)

cur' :: Pointer -> Size -> Seq ListUI -> Maybe (Image, Int)
cur' (l, i) s ls = do
    (a, c, b) <- splitOn l ls
    let r = hCat . fmap rList
    let pre = r a
    let img = pre <|> rCurList s i c <|> r b
    return (img, imageWidth pre)
    
cur :: Pointer -> Size -> Seq ListUI -> (Image, Int)
cur c s ls = fromMaybe (emptyImage, 0) (cur' c s ls)

calcOffset :: Int -> Int -> Int
calcOffset pivot n = if n > pivot then pivot - n else 0

offset :: Size -> Int -> Int
offset (w, _) = calcOffset (w `div` 3)

-- draws the screen
pic :: State -> Picture
pic s = Picture (Cursor 0 0) [translateX o img] ClearBackground
    where ls = present <$> lists s
          sz = size s
          (img, w) = cur (current s) sz ls
          o = offset sz w

-- styling
task :: TaskUI -> Image
task = img attrNormal 

currentTask :: TaskUI -> Image
currentTask = img attrCurrent

renderTitle :: Bool -> TaskUI -> Image
renderTitle current = marginTop . img (if current then attrTitleSelected else attrTitle)

-- vty helpers
img :: Attr -> TaskUI -> Image
img a s = vertCat $ string a <$> s

hCat :: Seq Image -> Image
hCat = foldl (<|>) emptyImage

vCat :: Seq Image -> Image
vCat = foldl (<->) emptyImage
