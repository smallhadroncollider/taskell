module Actions (event) where

import State
import Graphics.Vty.Input.Events
import Keyboard

event :: Event -> State -> State
event e | isChar 'q' e = quit
        | isUp e || isChar 'k' e = previous
        | isDown e || isChar 'j' e =  next
        | isChar ' ' e = setCompleted
        | isChar '.' e = toggleShowCompleted
