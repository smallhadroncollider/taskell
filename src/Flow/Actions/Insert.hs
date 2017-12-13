module Flow.Actions.Insert (event) where

import Graphics.Vty.Input.Events
import Flow.State

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . (newItem =<<) . store
event (EvKey KEsc _) = (write =<<) . finishInsert
event (EvKey KBS _) = insertBS
event (EvKey (KChar char) _) = insertCurrent char
event _ = return 
