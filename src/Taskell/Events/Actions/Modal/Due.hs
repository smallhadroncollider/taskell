{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Taskell.Events.Actions.Modal.Due
    ( event
    , events
    ) where

import ClassyPrelude

import Graphics.Vty.Input.Events         (Event (EvKey))
import Taskell.Events.Actions.Types      as A (ActionType (..))
import Taskell.Events.State              (normalMode, store, write)
import Taskell.Events.State.Modal.Detail (showDetail)
import Taskell.Events.State.Modal.Due    (clearDate, goto, next, previous)
import Taskell.Events.State.Types        (Stateful)
import Taskell.IO.Keyboard.Types         (Actions)

events :: Actions
events =
    [ (A.Previous, previous)
    , (A.Next, next)
    , (A.Detail, (showDetail =<<) . goto)
    , (A.ClearDate, (write =<<) . (clearDate =<<) . store)
    ]

event :: Event -> Stateful
event (EvKey _ _) = normalMode
event _           = pure
