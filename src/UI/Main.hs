module UI.Main where

import Graphics.Vty
import Flow.State (State, tasks, getIndex, getCurrentList)
import UI.List (list)
import Data.Sequence (mapWithIndex)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrNothing :: Attr
attrNothing = defAttr `withStyle` dim 

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

marginRight :: Image -> Image
marginRight = pad 0 0 10 0

nothing :: Image
nothing = string attrNothing "No items"

-- creates the title element
title :: Image
title = marginBottom $ string attrTitle "[Taskell]"

-- draws the screen
pic :: State -> Picture
pic s = picForImage $ title <-> if null lists then nothing else foldr1 (<|>) lists
    where ts = tasks s
          i = getIndex s
          l = getCurrentList s
          lists = mapWithIndex (\index task -> marginRight $ list task (index == l) i index) ts
