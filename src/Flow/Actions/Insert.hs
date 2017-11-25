module Flow.Actions.Insert (event) where

import Graphics.Vty.Input.Events
import Flow.State

event :: Event -> Stateful
event (EvKey KEnter _) = newItem
event (EvKey KEsc _) = finishInsert
event (EvKey KBS _) = insertBS
event (EvKey (KChar char) _) = insertCurrent char
event _ = return 
