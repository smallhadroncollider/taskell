module Events.Actions.Search (event) where

import Graphics.Vty.Input.Events
import Events.State

import qualified Events.Actions.Normal as Normal

event' :: Event -> Stateful
event' (EvKey KEnter _) = searchEntered
event' (EvKey KBS _) = searchBS
event' (EvKey (KChar char) _) = searchChar char
event' _ = return

event :: Event -> Stateful
event (EvKey KEsc _) s = normalMode s
event e s = case mode s of
    Search ent _ -> (if ent then event' else Normal.event) e s
    _ -> return s
