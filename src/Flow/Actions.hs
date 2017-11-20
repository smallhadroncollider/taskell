module Flow.Actions (event) where

import Graphics.Vty.Input.Events
import Flow.State
import Flow.Keyboard

event' :: Event -> State -> State
event' e
    | isChar 'q' e = quit
    | isChar 'a' e = newAndStartInsert
    | isChar 'e' e = startInsert
    | isUp e || isChar 'k' e = previous
    | isDown e || isChar 'j' e =  next
    | isLeft e || isChar 'h' e = switch 
    | isRight e || isChar 'l' e = switch 
    | isChar ' ' e = toggleCompleted
    | otherwise = id

insertEvent :: Event -> State -> State
insertEvent e
    | isEnter e || isEsc e = finishInsert
    | isBS e = insertBS
    | otherwise = (maybe id insertCurrent (char e))

event :: Event -> State -> State
event e s
    | insert s = insertEvent e s 
    | otherwise = event' e s
