module UI.Modal (
    showModal
) where

import Events.State (State, Mode(..), ModalType(..), mode)
import Brick

import UI.Types (ResourceName)
import UI.Modal.Internal (surround)
import UI.Modal.Help (help)
import UI.Modal.SubTasks (st)
import UI.Modal.MoveTo (moveTo)

getModal :: State -> ModalType -> Widget ResourceName
getModal s t = case t of
    Help -> surround help
    SubTasks _ _ -> surround $ st s
    MoveTo -> surround $ moveTo s

showModal :: State -> [Widget ResourceName] -> [Widget ResourceName]
showModal s view = case mode s of
    Modal t -> getModal s t : view
    _ -> view
