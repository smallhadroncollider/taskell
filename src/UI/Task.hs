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
present :: Int -> Task -> Image
present i t = string style ("• " ++ s)
    where
        s = description t
        style = if completed t then attrDone else attrTask
