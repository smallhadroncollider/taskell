{-# LANGUAGE NoImplicitPrelude #-}

module Events.Actions.Modal.MoveTo
    ( event
    ) where

import ClassyPrelude
import Events.State
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events

event :: Event -> Stateful
event (EvKey KEsc _)      = normalMode
event (EvKey KEnter _)    = normalMode
event (EvKey (KChar c) _) = (normalMode =<<) . (write =<<) . (moveTo c =<<) . store
event _                   = pure
