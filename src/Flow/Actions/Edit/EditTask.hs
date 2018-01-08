module Flow.Actions.Edit.EditTask (event) where

import Graphics.Vty.Input.Events
import Flow.State (Stateful, write, normalMode, insertBS, insertCurrent)

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . normalMode
event (EvKey KEsc _) = (write =<<) . normalMode
event (EvKey KBS _) = insertBS
event (EvKey (KChar char) _) = insertCurrent char
event _ = return
