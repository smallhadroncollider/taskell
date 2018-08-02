{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Search (event) where

import ClassyPrelude

import Control.Lens ((&), (^.), (.~))

import Graphics.Vty.Input.Events
import Events.State
import Events.State.Types (Stateful, mode)
import Events.State.Types.Mode (Mode(Search))
import qualified UI.Field as F (event)

import qualified Events.Actions.Normal as Normal

search :: Event -> Stateful
search (EvKey KEnter _) s = searchEntered s
search e s = return $ case s ^. mode of
    Search ent field -> s & mode .~ Search ent (F.event e field)
    _ -> s

event :: Event -> Stateful
event (EvKey KEsc _) s = normalMode s
event e s = case s ^. mode of
    Search ent _ -> (if ent then search else Normal.event) e s
    _ -> return s
