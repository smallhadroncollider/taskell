module Flow.Actions (event) where

import Graphics.Vty.Input.Events
import Flow.State
import Flow.Keyboard

event :: Event -> State -> State
event e | isChar 'q' e = quit
        | isUp e || isChar 'k' e = previous
        | isDown e || isChar 'j' e =  next
        | isLeft e || isChar 'h' e = switch 
        | isRight e || isChar 'l' e = switch 
        | isChar ' ' e = setCompleted
        | otherwise = id
