module UI.Task where

import Graphics.Vty
import Data.Taskell.Task
import Config (width)

-- styles
attrTask :: Attr
attrTask = defAttr `withForeColor` magenta

attrCurrent :: Attr
attrCurrent = defAttr `withForeColor` blue

trunc :: String -> String
trunc s = if length s > width then take (width - 3) s ++ "..." else s

-- style a task
present :: Bool -> Int -> Int -> Task -> Image
present current index i t = if imageWidth img < width then resizeWidth width img else img
    where
        s = "• " ++ description t
        style = if current && index == i then attrCurrent else attrTask 
        s' = if current then s else trunc s 
        img = string style s'
