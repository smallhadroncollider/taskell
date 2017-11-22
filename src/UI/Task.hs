module UI.Task where

import Graphics.Vty
import Data.Taskell.Task

-- styles
attrTask :: Attr
attrTask = defAttr `withForeColor` magenta

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` blue

-- style a task
present :: Bool -> Int -> Int -> Task -> Image
present current index i t = string style ("• " ++ s)
    where
        s = description t
        style = if current && index == i then attrCurrent else attrTask 
