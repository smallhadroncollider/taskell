module UI.Main where

import Graphics.Vty
import Data.Foldable (toList)

import Data.Sequence (Seq, (<|), mapWithIndex)

import Flow.State (State, lists, current)

import UI.Styles

import Config (width)
import Data.Taskell.String (wrap)
import Data.Taskell.Task (description)
import Data.Taskell.List (List, tasks)

present :: List -> Seq [String] 
present l = wrap width . description <$> tasks l 

-- styling
task :: [String] -> Image
task = img attrNormal 

currentTask :: [String] -> Image
currentTask = img attrCurrent

render :: (Int, Int) -> (Int, Int) -> ([String] -> Image)
render a b = if a == b then currentTask else task

-- draws the screen
pic :: State -> Picture
pic s = Picture NoCursor [image] ClearBackground
    where ls = present <$> lists s
          r' = render (current s)
          parse i = margin . vCat . mapWithIndex (\j -> marginTop . r' (i, j))
          image = hCat $ mapWithIndex parse ls

-- vty helpers
img :: Attr -> [String] -> Image
img a s = vertCat $ string a <$> s

hCat :: Seq Image -> Image
hCat = foldl (<|>) emptyImage

vCat :: Seq Image -> Image
vCat = foldl (<->) emptyImage
