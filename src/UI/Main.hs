module UI.Main where

import Graphics.Vty
import Flow.State
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
pic s = picForImage $ title <-> todo <|> marginLeft done
    where
        i = getIndex s
        todo = list "To Do" (getList s == ToDo) i (getToDo s)
        done = list "Done" (getList s == Done) i (getDone s)
