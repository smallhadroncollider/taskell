module Flow.Actions.EditList (event) where

import Graphics.Vty.Input.Events
import Flow.State (Stateful, write, finishInsert, editListBS, editListChar)

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . finishInsert
event (EvKey KEsc _) = (write =<<) . finishInsert
event (EvKey KBS _) = editListBS
event (EvKey (KChar char) _) = editListChar char
event _ = return
