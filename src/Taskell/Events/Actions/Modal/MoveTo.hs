module Taskell.Events.Actions.Modal.MoveTo
    ( event
    ) where

import ClassyPrelude
import Graphics.Vty.Input.Events
import Taskell.Events.State
import Taskell.Events.State.Types (Stateful)

event :: Event -> Stateful
event (EvKey KEsc _)      = normalMode
event (EvKey KEnter _)    = normalMode
event (EvKey (KChar c) _) = (normalMode =<<) . (write =<<) . (moveTo c =<<) . store
event _                   = pure
