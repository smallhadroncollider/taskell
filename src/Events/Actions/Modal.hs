module Events.Actions.Modal (event) where

import Graphics.Vty.Input.Events
import Events.State

import qualified Events.Actions.Modal.Help as Help
import qualified Events.Actions.Modal.SubTasks as SubTasks

event :: Event -> Stateful

event (EvKey (KChar 'q') _) s = quit s

event e s = case mode s of
    Modal Help -> Help.event e s
    Modal SubTasks -> SubTasks.event e s
    _ -> return s
