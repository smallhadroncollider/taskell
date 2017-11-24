module UI.Main where

import Graphics.Vty
import Flow.State (State, tasks, current, getCursor, getNewList, size)
import UI.List (list)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, mapWithIndex)
import Data.Foldable (toList)
import qualified Config as C

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrNothing :: Attr
attrNothing = defAttr `withStyle` dim 

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

marginTop :: Image -> Image
marginTop = pad 0 1 0 0

marginRight :: Image -> Image
marginRight = pad 0 0 C.padding 0

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

makeCursor :: Int -> [Image] -> Image -> (Int, Int, Int) -> Cursor
makeCursor o im t (l, i, len) = Cursor (col + len + bulletLen + o) (i + imageHeight t + listHeadHeight)
    where bulletLen = 2
          listHeadHeight = 1
          col = width l im

calculateCursor :: Int -> [Image] -> Image -> State -> Cursor
calculateCursor o im t = maybe NoCursor (makeCursor o im t) . getCursor

-- draws the screen
pic :: State -> Picture
pic s = Picture (calculateCursor offset' lists t s) [image] ClearBackground
    where ts = tasks s
          current' = current s
          lists = map marginRight . toList $ mapWithIndex (list s) ts
          t = title s
          all = horizCat lists
          (w, h) = size s
          col = C.width + C.padding
          offset = (negate col * fst current') + (w `div` 2) - (col `div` 2)
          offset' = if offset > 0 then 0 else offset
          image = t <-> if null lists then nothing else translateX offset' all
