{-# LANGUAGE NoImplicitPrelude #-}

module Events.Actions.Search
    ( event
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import           Events.State
import           Events.State.Types        (Stateful, mode)
import           Events.State.Types.Mode   (Mode (Search))
import           Graphics.Vty.Input.Events
import qualified UI.Draw.Field             as F (event)

event :: Event -> Stateful
event (EvKey KEsc _) s = clearSearch =<< normalMode s
event (EvKey KEnter _) s = normalMode s
event e s =
    case s ^. mode of
        Search -> appendSearch (F.event e) s
        _      -> pure s
