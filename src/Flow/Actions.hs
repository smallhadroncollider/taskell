module Flow.Actions (event) where

import Graphics.Vty.Input.Events
import Flow.State
import Flow.Keyboard

event :: Event -> State -> State
event e | isChar 'q' e = quit
        | isUp e || isChar 'k' e = previous
        | isDown e || isChar 'j' e =  next
        | isChar ' ' e = setCompleted
        | isChar '.' e = toggleShowCompleted
        | otherwise = id
