{-# LANGUAGE OverloadedLists #-}

module Taskell.Events.Actions.Modal.Help
    ( event
    , events
    ) where

import ClassyPrelude
import Graphics.Vty.Input.Events
import Taskell.Events.Actions.Types as A (ActionType (..))
import Taskell.Events.State
import Taskell.Events.State.Types   (Stateful)
import Taskell.IO.Keyboard.Types    (Actions)

events :: Actions
events = [(A.Quit, quit)]

event :: Event -> Stateful
event (EvKey _ _) = normalMode
event _           = pure
