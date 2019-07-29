{-# LANGUAGE NoImplicitPrelude #-}

module Events.Actions.Search
    ( event
    ) where

import ClassyPrelude

import Control.Lens ((&), (.~), (^.))

import           Events.State
import           Events.State.Types        (Stateful, mode)
import           Events.State.Types.Mode   (Mode (Search))
import           Graphics.Vty.Input.Events
import qualified UI.Field                  as F (event)

search :: Event -> Stateful
search (EvKey KEnter _) s = searchEntered s
search e s =
    pure $
    case s ^. mode of
        Search ent field -> s & mode .~ Search ent (F.event e field)
        _                -> s

event :: (Event -> Stateful) -> Event -> Stateful
event _ (EvKey KEsc _) s = normalMode s
event fb e s =
    case s ^. mode of
        Search True _  -> search e s
        Search False _ -> fb e s
        _              -> pure s
