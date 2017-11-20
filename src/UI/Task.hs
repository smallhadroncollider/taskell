module UI.Task where

import Graphics.Vty
import Data.Taskell.Task

-- styles
attrTask :: Attr
attrTask = defAttr `withForeColor` magenta

attrDone :: Attr
attrDone = defAttr `withStyle` dim 

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` blue

-- style a task
present :: Bool -> Int -> Int -> Task -> Image
present current index i t = string style' ("• " ++ s)
    where
        s = description t
        style = if completed t then attrDone else attrTask
        style' = if current && index == i then attrCurrent else style
