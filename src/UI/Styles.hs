module UI.Styles where

import Graphics.Vty
import Config

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` blue

attrNoItems :: Attr
attrNoItems = defAttr `withStyle` dim 

attrNothing :: Attr
attrNothing = defAttr `withStyle` dim 

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

marginTop :: Image -> Image
marginTop = pad 0 1 0 0

marginRight :: Image -> Image
marginRight = pad 0 0 padding 0

attrTask :: Attr
attrTask = defAttr `withForeColor` magenta
