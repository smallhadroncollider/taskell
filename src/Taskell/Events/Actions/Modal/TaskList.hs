{-# LANGUAGE OverloadedLists #-}

module Taskell.Events.Actions.Modal.TaskList where

import ClassyPrelude
import Graphics.Vty.Input.Events (Event)
import Taskell.Events.Actions.Types as A (ActionType (..))
import Taskell.Events.State (normalMode, quit)
import Taskell.Events.State.Types (Stateful)
import Taskell.IO.Keyboard.Types (Actions)

events :: Actions
events = [(A.Quit, quit)]

event :: Event -> Stateful
event _ = normalMode
