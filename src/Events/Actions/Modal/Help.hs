{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Modal.Help
    ( event
    , events
    ) where

import ClassyPrelude
import Events.Actions.Types      (ActionType (..))
import Events.State
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events
import IO.Keyboard.Types         (Actions)

events :: Actions
events = [(AQuit, quit)]

event :: Event -> Stateful
event (EvKey _ _) = normalMode
event _           = pure
