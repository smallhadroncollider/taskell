module UI.Main where

import Graphics.Vty
import Flow.State (State, tasks, current, getCursor)
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

marginRight :: Image -> Image
marginRight = pad 0 0 5 0

nothing :: Image
nothing = string attrNothing "No items"

-- creates the title element
title :: Image
title = marginBottom $ string attrTitle "[Taskell]"

-- cursor code
width :: Int -> [Image] -> Int
width i = sum . map imageWidth . take i

makeCursor :: [Image] -> (Int, Int, Int) -> Cursor
makeCursor im (l, i, len) = Cursor (col + len + bulletLen) (i + imageHeight title + listHeadHeight)
    where bulletLen = 2
          listHeadHeight = 1
          col = width l im

calculateCursor :: [Image] -> State -> Cursor
calculateCursor im = maybe NoCursor (makeCursor im) . getCursor

-- draws the screen
pic :: State -> Picture
pic s = Picture (calculateCursor lists s) [image] ClearBackground
    where ts = tasks s
          lists = map marginRight . toList $ mapWithIndex (list $ current s) ts
          image = title <-> if null lists then nothing else horizCat lists
