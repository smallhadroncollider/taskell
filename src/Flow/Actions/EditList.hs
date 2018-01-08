module Flow.Actions.EditList (event) where

import Graphics.Vty.Input.Events
import Flow.State (Stateful, write, normalMode, editListBS, editListChar)

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . normalMode
event (EvKey KEsc _) = (write =<<) . normalMode
event (EvKey KBS _) = editListBS
event (EvKey (KChar char) _) = editListChar char
event _ = return
