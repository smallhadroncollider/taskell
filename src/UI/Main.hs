module UI.Main where

import Graphics.Vty
import Flow.State (State, tasks, current, getCursor, getNewList)
import UI.List (list)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, mapWithIndex)
import Data.Foldable (toList)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrNothing :: Attr
attrNothing = defAttr `withStyle` dim 

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

marginTop :: Image -> Image
marginTop = pad 0 1 0 0

marginRight :: Image -> Image
marginRight = pad 0 0 5 0

nothing :: Image
nothing = string attrNothing "No items"

newList :: State -> Image
newList s = maybe emptyImage img n 
    where n = getNewList s
          img s = marginTop $ string attrNothing ("New List: " ++ s)

-- creates the title element
title :: State -> Image
title s = marginBottom $ string attrTitle "[Taskell]" <-> newList s

-- cursor code
width :: Int -> [Image] -> Int
width i = sum . map imageWidth . take i

makeCursor :: [Image] -> Image -> (Int, Int, Int) -> Cursor
makeCursor im t (l, i, len) = Cursor (col + len + bulletLen) (i + imageHeight t + listHeadHeight)
    where bulletLen = 2
          listHeadHeight = 1
          col = width l im

calculateCursor :: [Image] -> Image -> State -> Cursor
calculateCursor im t = maybe NoCursor (makeCursor im t) . getCursor

-- draws the screen
pic :: State -> Picture
pic s = Picture (calculateCursor lists t s) [image] ClearBackground
    where ts = tasks s
          lists = map marginRight . toList $ mapWithIndex (list $ current s) ts
          t = title s
          image = t <-> if null lists then nothing else horizCat lists
