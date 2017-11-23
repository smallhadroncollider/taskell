module UI.Main where

import Graphics.Vty
import Flow.State (State, tasks, getIndex, getCurrentList, getCursor)
import UI.List (list)
import Prelude hiding (take)
import Data.Sequence (Seq, mapWithIndex, take)
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
width :: Int -> Seq Image -> Int
width i = sum . map imageWidth . toList . take i

makeCursor :: (Int, Int, Int) -> Seq Image -> Cursor
makeCursor (l, i, len) im = Cursor (col + len + bulletLen) (i + imageHeight title + listHeadHeight)
    where bulletLen = 2
          listHeadHeight = 1
          col = width l im

calculateCursor :: State -> Seq Image -> Cursor
calculateCursor s im = case getCursor s of
    Just c -> makeCursor c im
    _ -> NoCursor 

-- draws the screen
pic :: State -> Picture
pic s = Picture (calculateCursor s lists) [image] ClearBackground
    where ts = tasks s
          i = getIndex s
          l = getCurrentList s
          lists = mapWithIndex (\index task -> marginRight $ list task (index == l) i index) ts
          image = title <-> if null lists then nothing else foldr1 (<|>) lists
