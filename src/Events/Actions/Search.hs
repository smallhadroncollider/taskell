{-# LANGUAGE NoImplicitPrelude #-}

module Events.Actions.Search
    ( event
    ) where

import ClassyPrelude

import Control.Lens ((&), (.~), (^.))

import           Events.State
import           Events.State.Types        (Stateful, mode, searchTerm)
import           Events.State.Types.Mode   (Mode (Search))
import           Graphics.Vty.Input.Events
import qualified UI.Field                  as F (blankField, event)

search :: Event -> Stateful
search (EvKey KEnter _) s = normalMode s
search e s =
    pure $
    case s ^. mode of
        Search -> do
            let field = s ^. searchTerm
            case field of
                Nothing -> s & searchTerm .~ Just (F.event e F.blankField)
                Just f  -> s & searchTerm .~ Just (F.event e f)
        _ -> s

event :: Event -> Stateful
event (EvKey KEsc _) s = clearSearch =<< normalMode s
event e s              = search e s
