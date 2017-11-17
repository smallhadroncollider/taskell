module Draw (
    render
) where

import Graphics.Vty
import Task (Task, Tasks, description, completed)
import State
import Data.Sequence (mapWithIndex) 
import Data.Foldable (toList)
import Actions

-- styles
attrTask :: Attr
attrTask = defAttr `withForeColor` magenta

attrDone :: Attr
attrDone = defAttr `withStyle` dim 

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` blue

attrTitle :: Attr
attrTitle = defAttr `withForeColor` green

marginBottom :: Image -> Image
marginBottom = pad 0 0 0 1

-- style a task
bullet :: Int -> Int -> Task -> Image
bullet cur i t = string style' ("• " ++ s ++ tick)
    where
        s = description t
        tick = if completed t then " ✓" else ""
        style = if completed t then attrDone else attrTask
        style' = if cur == i then attrCurrent else style

-- creates the title element
title = marginBottom $ string attrTitle "[Taskell]"

-- draws the screen
pic :: State -> Picture
pic state = picForImage $ title <-> imgs
    where
        bullet' = bullet $ current state
        imgs = vertCat $ toList $ mapWithIndex bullet' $ tasks state

-- the draw loop
draw :: Vty -> State -> IO ()
draw vty state = do
     update vty $ pic state
     e <- nextEvent vty
     let state' = event e state
     
     if running state'
        then draw vty state'
        else shutdown vty

-- setup vty and start the draw loop
render :: State -> IO ()
render state = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    draw vty state
