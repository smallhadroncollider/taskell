module Flow.Actions.Edit (event) where

import Graphics.Vty.Input.Events (Event)
import Flow.State (Stateful, Mode(..), EditMode(..), mode)

import qualified Flow.Actions.Edit.CreateTask as CreateTask
import qualified Flow.Actions.Edit.EditTask as EditTask
import qualified Flow.Actions.Edit.CreateList as CreateList
import qualified Flow.Actions.Edit.EditList as EditList

event :: Event -> Stateful

-- for other events pass through to relevant modules
event e s = case mode s of
    Edit CreateTask -> CreateTask.event e s
    Edit EditTask -> EditTask.event e s
    Edit EditList -> EditList.event e s
    Edit (CreateList _) -> CreateList.event e s
    _ -> return s
