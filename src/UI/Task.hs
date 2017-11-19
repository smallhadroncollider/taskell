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
present :: Int -> Int -> Task -> Image
present cur i t = string style' ("• " ++ s ++ tick)
    where
        s = description t
        tick = if completed t then " ✓" else ""
        style = if completed t then attrDone else attrTask
        style' = if cur == i then attrCurrent else style
