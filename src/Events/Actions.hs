{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions (event) where

import ClassyPrelude

import Control.Lens ((^.))

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
event' e state = case state ^. mode of
    Normal -> Normal.event e state
    Search {} -> Search.event e state
    Insert {} -> Insert.event e state
    Modal {} -> Modal.event e state
    _ -> return state

-- returns new state if successful
event :: Event -> State -> State
event e state = fromMaybe state $ event' e state
