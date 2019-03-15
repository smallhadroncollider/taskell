{-# LANGUAGE NoImplicitPrelude #-}

module Events.Actions.Modal
    ( event
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Events.State.Types        (Stateful, mode)
import Events.State.Types.Mode   (ModalType (..), Mode (Modal))
import Graphics.Vty.Input.Events

import qualified Events.Actions.Modal.Detail as Detail
import qualified Events.Actions.Modal.Help   as Help
import qualified Events.Actions.Modal.MoveTo as MoveTo

event :: Event -> Stateful
event e s =
    case s ^. mode of
        Modal Help         -> Help.event e s
        Modal (Detail _ _) -> Detail.event e s
        Modal MoveTo       -> MoveTo.event e s
        _                  -> pure s
