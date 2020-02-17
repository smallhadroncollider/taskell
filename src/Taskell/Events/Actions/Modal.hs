module Taskell.Events.Actions.Modal
    ( event
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Graphics.Vty.Input.Events
import Taskell.Events.State.Types      (Stateful, mode)
import Taskell.Events.State.Types.Mode (ModalType (..), Mode (Modal))

import qualified Taskell.Events.Actions.Modal.Detail as Detail
import qualified Taskell.Events.Actions.Modal.Due    as Due
import qualified Taskell.Events.Actions.Modal.Help   as Help
import qualified Taskell.Events.Actions.Modal.MoveTo as MoveTo

event :: Event -> Stateful
event e s =
    case s ^. mode of
        Modal Help      -> Help.event e s
        Modal Detail {} -> Detail.event e s
        Modal MoveTo    -> MoveTo.event e s
        Modal Due {}    -> Due.event e s
        _               -> pure s
