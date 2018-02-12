module Events.Actions.Modal (event) where

import Graphics.Vty.Input.Events
import Events.State

event :: Event -> Stateful

event (EvKey (KChar 'q') _) = quit
event (EvKey _ _) = normalMode

event _ = return
