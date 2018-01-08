module Flow.Actions.Edit.CreateList (event) where

import Graphics.Vty.Input.Events
import Flow.State (Stateful, write, createListFinish, normalMode, createListBS, createListChar)

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . createListFinish
event (EvKey KEsc _) = normalMode
event (EvKey KBS _) = createListBS
event (EvKey (KChar char) _) = createListChar char
event _ = return
