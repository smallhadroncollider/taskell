module Flow.Actions.Insert (event) where

import Graphics.Vty.Input.Events (Event(..), Key(..))
import Flow.State (Stateful, Mode(..), InsertMode(..), mode)

import qualified Flow.Actions.Insert.Task as Task
import qualified Flow.Actions.Insert.List as List

event :: Event -> Stateful

event (EvKey (KChar '\t') _) s = return s

-- for other events pass through to relevant modules
event e s = case mode s of
    Insert CreateTask -> Task.event e s
    Insert EditTask -> Task.event e s

    Insert EditList -> List.event e s
    Insert (CreateList _) -> List.event e s

    _ -> return s
