module UI.Styles where

import Graphics.Vty
import Config (padding)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

attrCurrentTitle :: Attr
attrCurrentTitle = defAttr `withForeColor` blue

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` magenta

attrNormal :: Attr
attrNormal = defAttr `withStyle` dim 

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

marginTop :: Image -> Image
marginTop = pad 0 1 0 0

margin :: Image -> Image
margin = pad padding 0 padding 0
