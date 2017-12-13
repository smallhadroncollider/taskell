module Flow.Actions.Normal (event) where

import Graphics.Vty.Input.Events
import Data.Char (isDigit)
import Flow.State

-- Normal 
event :: Event -> Stateful

-- quit
event (EvKey (KChar 'q') _) = quit

-- add/edit
event (EvKey (KChar 'e') _) = (startInsert =<<) . store
event (EvKey (KChar 'i') _) = (startInsert =<<) . store
event (EvKey (KChar 'a') _) = (startInsert =<<) . (newItem =<<) . store
event (EvKey (KChar 'O') _) = (startInsert =<<) . (above  =<<) . store
event (EvKey (KChar 'o') _) = (startInsert =<<) . (below =<<) . store

-- add list
event (EvKey (KChar 'N') _) = (createListStart =<<) . store
event (EvKey (KChar 'X') _) = (write =<<) . (deleteCurrentList =<<) . store

-- navigation
event (EvKey (KChar 'k') _) = previous
event (EvKey (KChar 'j') _) = next
event (EvKey (KChar 'h') _) = left
event (EvKey (KChar 'l') _) = right
event (EvKey (KChar 'G') _) = bottom

-- moving items
event (EvKey (KChar 'K') _) = (write =<<) . (up =<<) . store
event (EvKey (KChar 'J') _) = (write =<<) . (down =<<) . store
event (EvKey (KChar 'H') _) = (write =<<) . (moveLeft =<<) . store
event (EvKey (KChar 'L') _) = (write =<<) . (moveRight =<<) . store
event (EvKey (KChar ' ') _) = (write =<<) . (moveRight =<<) . store

-- removing items
event (EvKey (KChar 'D') _) = (write =<<) . (delete =<<) . store

-- undo
event (EvKey (KChar 'u') _) = (write =<<) . undo

-- moving lists
event (EvKey (KChar '>') _) = (write =<<) . (listRight =<<) . store
event (EvKey (KChar '<') _) = (write =<<) . (listLeft =<<) . store

-- selecting lists
event (EvKey (KChar n) _)
    | isDigit n = selectList n
    | otherwise = return

-- fallback
event _ = return
