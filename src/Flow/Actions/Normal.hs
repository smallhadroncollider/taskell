module Flow.Actions.Normal (event) where

import Graphics.Vty.Input.Events
import Data.Char (isDigit)
import Control.Monad
import Flow.State

-- Normal 
event :: Event -> Stateful

-- quit
event (EvKey (KChar 'q') _) = quit

-- add/edit
event (EvKey (KChar 'a') _) = (startInsert =<<) . newItem
event (EvKey (KChar 'e') _) = startInsert

-- add list
event (EvKey (KChar 'N') _) = createListStart
event (EvKey (KChar 'X') _) = deleteCurrentList

-- navigation
event (EvKey (KChar 'k') _) = previous
event (EvKey (KChar 'j') _) = next
event (EvKey (KChar 'h') _) = left
event (EvKey (KChar 'l') _) = right

-- moving items
event (EvKey (KChar 'K') _) = up
event (EvKey (KChar 'J') _) = down
event (EvKey (KChar 'H') _) = moveLeft
event (EvKey (KChar 'L') _) = moveRight
event (EvKey (KChar ' ') _) = moveRight

-- removing items
event (EvKey (KChar 'D') _) = delete

-- selecting lists
event (EvKey (KChar n) _)
    | isDigit n = selectList n
    | otherwise = return

event (EvResize w h) = setSize w h 

-- fallback
event _ = return
