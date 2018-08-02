{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Modal (event) where

import ClassyPrelude

import Control.Lens ((^.))

import Graphics.Vty.Input.Events
import Events.State.Types (Stateful, mode)
import Events.State.Types.Mode (Mode(Modal), ModalType(..))

import qualified Events.Actions.Modal.Help as Help
import qualified Events.Actions.Modal.MoveTo as MoveTo
import qualified Events.Actions.Modal.Detail as Detail

event :: Event -> Stateful

event e s = case s ^. mode of
    Modal Help -> Help.event e s
    Modal (Detail _ _) -> Detail.event e s
    Modal MoveTo -> MoveTo.event e s
    _ -> return s
