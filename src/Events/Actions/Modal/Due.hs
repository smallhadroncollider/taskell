{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Modal.Due
    ( event
    , events
    ) where

import ClassyPrelude

import Events.Actions.Types      as A (ActionType (..))
import Events.State              (normalMode, store, write)
import Events.State.Modal.Detail (showDetail)
import Events.State.Modal.Due    (clearDate, goto, next, previous)
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events (Event (EvKey))
import IO.Keyboard.Types         (Actions)

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
