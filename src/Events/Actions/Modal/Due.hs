{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Modal.Due
    ( event
    ) where

import ClassyPrelude

import Events.State
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events

event :: Event -> Stateful
event (EvKey _ _) = normalMode
event _           = pure
