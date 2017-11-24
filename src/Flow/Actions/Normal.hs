module Flow.Actions.Normal (normal) where

import Graphics.Vty.Input.Events
import Flow.State

-- Normal 
normal :: Event -> Stateful

-- quit
normal (EvKey (KChar 'q') _) = quit

-- add/edit
normal (EvKey (KChar 'a') _) = startInsert . newItem
normal (EvKey (KChar 'e') _) = startInsert

-- navigation
normal (EvKey (KChar 'k') _) = previous
normal (EvKey (KChar 'j') _) = next
normal (EvKey (KChar 'h') _) = left
normal (EvKey (KChar 'l') _) = right

-- moving items
normal (EvKey (KChar 'K') _) = up
normal (EvKey (KChar 'J') _) = down
normal (EvKey (KChar 'H') _) = moveLeft
normal (EvKey (KChar 'L') _) = moveRight
normal (EvKey (KChar ' ') _) = moveRight

-- removing items
normal (EvKey (KChar 'D') _) = delete

normal (EvResize w h) = setSize w h

-- fallback
normal _ = id
