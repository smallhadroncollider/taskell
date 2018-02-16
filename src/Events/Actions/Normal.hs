module Events.Actions.Normal (event) where

import Graphics.Vty.Input.Events
import Data.Char (isDigit)
import Events.State
import Events.State.Modal.SubTasks (showSubTasks)

-- Normal
event :: Event -> Stateful

-- quit
event (EvKey (KChar 'q') _) = quit

-- add/edit
event (EvKey (KChar 'e') _) = (startEdit =<<) . store
event (EvKey (KChar 'A') _) = (startEdit =<<) . store
event (EvKey (KChar 'i') _) = (startEdit =<<) . store
event (EvKey (KChar 'C') _) = (startEdit =<<) . (clearItem =<<) . store
event (EvKey (KChar 'a') _) = (startCreate =<<) . (newItem =<<) . store
event (EvKey (KChar 'O') _) = (startCreate =<<) . (above  =<<) . store
event (EvKey (KChar 'o') _) = (startCreate =<<) . (below =<<) . store

-- add list
event (EvKey (KChar 'N') _) = (createListStart =<<) . store
event (EvKey (KChar 'E') _) = (editListStart =<<) . store
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
event (EvKey (KChar 'H') _) = (write =<<) . (bottom =<<) . (left =<<) . (moveLeft =<<) . store
event (EvKey (KChar 'L') _) = (write =<<) . (bottom =<<) . (right =<<) . (moveRight =<<) . store
event (EvKey (KChar ' ') _) = (write =<<) . (moveRight =<<) . store

-- removing items
event (EvKey (KChar 'D') _) = (write =<<) . (delete =<<) . store

-- undo
event (EvKey (KChar 'u') _) = (write =<<) . undo

-- moving lists
event (EvKey (KChar '>') _) = (write =<<) . (listRight =<<) . store
event (EvKey (KChar '<') _) = (write =<<) . (listLeft =<<) . store

-- search
event (EvKey (KChar '/') _) = searchMode

-- help
event (EvKey (KChar '?') _) = showHelp

-- subtasks
event (EvKey KEnter _) = showSubTasks

-- selecting lists
event (EvKey (KChar n) _)
    | isDigit n = selectList n
    | otherwise = return

-- fallback
event _ = return
