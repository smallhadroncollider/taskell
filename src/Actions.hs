module Actions (event) where

import State
import Graphics.Vty.Input.Events
import Keyboard (isChar)

-- is it a quit event
isQ :: Event -> Bool
isQ = isChar 'q'

event :: State -> Event -> State
event s e = if isQ e then quit s else s
