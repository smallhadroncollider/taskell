{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw.Mode where

import ClassyPrelude

import Events.State.Types.Mode (InsertType (..), ModalType (..), Mode (..))
import UI.Draw.Field           (Field)

getField :: Mode -> Maybe Field
getField (Insert _ _ f) = Just f
getField _              = Nothing

editingTitle :: Mode -> Bool
editingTitle (Insert IList _ _) = True
editingTitle _                  = False

moveTo :: Mode -> Bool
moveTo (Modal MoveTo) = True
moveTo _              = False
