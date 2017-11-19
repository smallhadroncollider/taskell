module UI.Main where

import Graphics.Vty

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

-- creates the title element
title = marginBottom $ string attrTitle "[Taskell]"
