{-# LANGUAGE NoImplicitPrelude #-}

module Events.Actions.Modal.Help
    ( event
    ) where

import ClassyPrelude
import Events.State
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events

event :: Event -> Stateful
event (EvKey (KChar 'q') _) = quit
event (EvKey _ _)           = normalMode
event _                     = pure
