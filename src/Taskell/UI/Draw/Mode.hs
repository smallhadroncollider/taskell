module Taskell.UI.Draw.Mode where

import ClassyPrelude

import Taskell.Events.State.Types.Mode (InsertType (..), ModalType (..), Mode (..))
import Taskell.UI.Draw.Field           (Field)

getField :: Mode -> Maybe Field
getField (Insert _ _ f) = Just f
getField _              = Nothing

editingTitle :: Mode -> Bool
editingTitle (Insert IList _ _) = True
editingTitle _                  = False

moveTo :: Mode -> Bool
moveTo (Modal MoveTo) = True
moveTo _              = False
