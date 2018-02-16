module Events.Actions.Modal.SubTasks (event) where

import Graphics.Vty.Input.Events
import Events.State

event :: Event -> Stateful
event (EvKey KEsc _) = normalMode
event _ = return
