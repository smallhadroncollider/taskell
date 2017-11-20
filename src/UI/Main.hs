module UI.Main where

import Graphics.Vty
import Flow.State (State, getDone, getToDo)
import UI.List (list)

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

marginLeft :: Image -> Image
marginLeft = pad 15 0 0 0

-- creates the title element
title :: Image
title = marginBottom $ string attrTitle "[Taskell]"

-- draws the screen
pic :: State -> Picture
pic state = picForImage $ title <-> todo <|> marginLeft done
    where
        todo = list "To Do" $ getToDo state
        done = list "Done" $ getDone state
