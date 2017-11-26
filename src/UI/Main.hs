module UI.Main where

import Graphics.Vty
import Data.Foldable (toList)

import Prelude hiding (take)
import Data.Sequence (Seq, mapWithIndex, take)

import Flow.State (State, lists, current, size)

import UI.Styles

import Config (width, padding)
import Data.Taskell.String (wrap)
import Data.Taskell.Task (description)
import Data.Taskell.List (List, tasks, title)

present :: List -> ([String], Seq [String])
present l = (wrap width (title l), wrap width . description <$> tasks l)

-- draws the screen
pic :: State -> Picture
pic s = Picture (Cursor x' 0) [translateX offset $ hCat lists'] ClearBackground
    where ls = present <$> lists s
          (l, i) = current s
          r' = render (l, i)
          parse i (t, ts) = margin $ renderTitle (i == l) t <-> (vCat . mapWithIndex (\j -> marginTop . r' (i, j))) ts
          lists' = mapWithIndex parse ls
          x = padding + sum (imageWidth <$> take l lists')
          (w, h) = size s
          pivot = w `div` 3
          offset = if x > pivot then pivot - x else 0
          x' = x + offset

-- styling
task :: [String] -> Image
task = img attrNormal 

currentTask :: [String] -> Image
currentTask = img attrCurrent

renderTitle :: Bool -> [String] -> Image
renderTitle current = marginTop . img (if current then attrCurrent else attrTitle)

render :: (Int, Int) -> (Int, Int) -> ([String] -> Image)
render a b = if a == b then currentTask else task

-- vty helpers
img :: Attr -> [String] -> Image
img a s = vertCat $ string a <$> s

hCat :: Seq Image -> Image
hCat = foldl (<|>) emptyImage

vCat :: Seq Image -> Image
vCat = foldl (<->) emptyImage
