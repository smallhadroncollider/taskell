module Flow.Actions (event) where

import Graphics.Vty.Input.Events
import Flow.State

-- non-insert mode events
event' :: Event -> State -> State

-- quit
event' (EvKey (KChar 'q') _) = quit

-- add/edit
event' (EvKey (KChar 'a') _) = startInsert . newItem
event' (EvKey (KChar 'e') _) = startInsert

-- navigation
event' (EvKey (KChar 'k') _) = previous
event' (EvKey (KChar 'j') _) = next
event' (EvKey (KChar 'h') _) = switch
event' (EvKey (KChar 'l') _) = switch

-- moving items
event' (EvKey (KChar 'K') _) = up
event' (EvKey (KChar 'J') _) = down

-- removing items
event' (EvKey (KChar 'D') _) = delete

-- fallback
event' _ = id

----------------------------------------------

-- in insert mode
insertEvent :: Event -> State -> State
insertEvent (EvKey KEnter _) = finishInsert
insertEvent (EvKey KEsc _) = finishInsert
insertEvent (EvKey KBS _) = insertBS
insertEvent (EvKey (KChar char) _) = insertCurrent char
insertEvent _ = id

-- detect if insert mode
event :: Event -> State -> State
event e s = case mode s of
    Insert -> insertEvent e s
    _ -> event' e s
