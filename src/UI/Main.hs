module UI.Main where

import Graphics.Vty
import Flow.State (State, tasks, getIndex, getCurrentList)
import Data.Map.Strict (mapWithKey, elems)
import UI.List (list)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

marginRight :: Image -> Image
marginRight = pad 0 0 10 0

-- creates the title element
title :: Image
title = marginBottom $ string attrTitle "[Taskell]"

-- draws the screen
pic :: State -> Picture
pic s = picForImage $ title <-> foldr1 (<|>) (elems lists)
    where ts = tasks s
          i = getIndex s
          l = getCurrentList s
          lists = mapWithKey (\k t -> marginRight $ list k (l == k) i t) ts
