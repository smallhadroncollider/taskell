{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions (event) where

import ClassyPrelude

import Graphics.Vty.Input.Events (Event(..))

import Events.State.Types (State, Stateful, mode)
import Events.State.Types.Mode (Mode(..))

import qualified Events.Actions.Normal as Normal
import qualified Events.Actions.Search as Search
import qualified Events.Actions.Insert as Insert
import qualified Events.Actions.Modal as Modal

-- takes an event and returns a Maybe State
event' :: Event -> Stateful

-- for other events pass through to relevant modules
event' e s = case mode s of
    Normal -> Normal.event e s
    Search {} -> Search.event e s
    Insert {} -> Insert.event e s
    Modal {} -> Modal.event e s
    _ -> return s

-- returns new state if successful
event :: Event -> State -> State
event e s = fromMaybe s $ event' e s
