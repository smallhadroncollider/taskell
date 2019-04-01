{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Modal.Help
    ( event
    , events
    ) where

import ClassyPrelude
import Events.State
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events

events :: Map Text Stateful
events = [("quit", quit)]

event :: Event -> Stateful
event (EvKey _ _) = normalMode
event _           = pure
