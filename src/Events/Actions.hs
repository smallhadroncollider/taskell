module Events.Actions (event) where

import Graphics.Vty.Input.Events (Event(..))

import Events.State (State, Stateful, Mode(..), mode, setSize)
import Data.Maybe (fromMaybe)

import qualified Events.Actions.Normal as Normal
import qualified Events.Actions.Search as Search
import qualified Events.Actions.Insert as Insert
import qualified Events.Actions.Modal as Modal

-- takes an event and returns a Maybe State
event' :: Event -> Stateful

-- always handle resize, no matter the mode
event' (EvResize w h) s = setSize w h s

-- for other events pass through to relevant modules
event' e s = case mode s of
    Normal -> Normal.event e s
    Search _ _ -> Search.event e s
    Insert _ -> Insert.event e s
    Modal _ -> Modal.event e s
    _ -> return s

-- returns new state if successful
event :: Event -> State -> State
event e s = fromMaybe s $ event' e s
