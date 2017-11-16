module Draw (
    render
) where

import Graphics.Vty
import Keyboard (isChar)
import Task (Task, Tasks, description, completed)
import State

-- styles
attrTask :: Attr
attrTask = defAttr `withForeColor` magenta

attrDone :: Attr
attrDone = defAttr `withStyle` dim 

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

-- style a task
bullet :: Task -> Image
bullet t = string style ("• " ++ s ++ tick)
    where
        s = description t
        tick = if completed t then " ✓" else ""
        style = if completed t then attrDone else attrTask

-- creates the title element
title = marginBottom $ string attrTitle "[Taskell]"

-- draws the screen
pic :: State -> Picture
pic state = picForImage $ title <-> imgs
    where imgs = vertCat $ map bullet $ tasks state

-- is it a quit event
quit :: Event -> Bool
quit = isChar 'q'

-- the draw loop
draw :: Vty -> State -> IO ()
draw vty state = do
     update vty $ pic state
     e <- nextEvent vty
     
     if quit e
         then shutdown vty
         else draw vty state

-- setup vty and start the draw loop
render :: State -> IO ()
render state = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    draw vty state
