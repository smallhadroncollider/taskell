module Flow.Actions (event) where

import Graphics.Vty.Input.Events (Event)

import Flow.State (State, Stateful, Mode(..), mode)
import Data.Maybe (fromMaybe)

import qualified Flow.Actions.Normal as Normal
import qualified Flow.Actions.Insert as Insert
import qualified Flow.Actions.CreateList as CreateList

event' :: Event -> Stateful 
event' e s = case mode s of
    Insert -> Insert.event e s
    Normal -> Normal.event e s
    CreateList _ -> CreateList.event e s
    _ -> return s

event :: Event -> State -> State
event e s = fromMaybe s $ event' e s 
