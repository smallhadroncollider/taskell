module Events.Actions.Modal.Help (event) where

import Graphics.Vty.Input.Events
import Events.State

event :: Event -> Stateful
event (EvKey _ _) = normalMode
event _ = return
