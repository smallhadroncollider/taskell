module Events.Actions.Modal.SubTasks (event) where

import Graphics.Vty.Input.Events
import Events.State
import Events.State.Modal.SubTasks

event :: Event -> Stateful
event (EvKey KEsc _) = normalMode
event (EvKey KEnter _) = normalMode
event (EvKey (KChar ' ') _) = (write =<<) . (setComplete =<<) . store
event _ = return
