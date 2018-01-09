module Flow.Actions (event) where

import Graphics.Vty.Input.Events (Event(..))

import Flow.State (State, Stateful, Mode(..), mode, setSize)
import Data.Maybe (fromMaybe)

import qualified Flow.Actions.Normal as Normal
import qualified Flow.Actions.Search as Search
import qualified Flow.Actions.Edit as Edit

-- takes an event and returns a Maybe State
event' :: Event -> Stateful

-- always handle resize, no matter the mode
event' (EvResize w h) s = setSize w h s

-- for other events pass through to relevant modules
event' e s = case mode s of
    Normal -> Normal.event e s
    Search _ _ -> Search.event e s
    Edit _ -> Edit.event e s
    _ -> return s

-- returns new state if successful
event :: Event -> State -> State
event e s = fromMaybe s $ event' e s
