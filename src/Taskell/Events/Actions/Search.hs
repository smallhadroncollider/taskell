module Taskell.Events.Actions.Search
    ( event
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import           Graphics.Vty.Input.Events
import           Taskell.Events.State
import           Taskell.Events.State.Types      (Stateful, mode)
import           Taskell.Events.State.Types.Mode (Mode (Search))
import qualified Taskell.UI.Draw.Field           as F (event)

event :: Event -> Stateful
event (EvKey KEsc _) s = clearSearch =<< normalMode s
event (EvKey KEnter _) s = normalMode s
event e s =
    case s ^. mode of
        Search -> appendSearch (F.event e) s
        _      -> pure s
