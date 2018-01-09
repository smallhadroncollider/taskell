module Flow.Actions.Search (event) where

import Graphics.Vty.Input.Events
import Data.Char (isDigit)
import Flow.State

import qualified Flow.Actions.Normal as Normal

entered :: Event -> Stateful
entered (EvKey KEsc _) = normalMode
entered e = Normal.event e
entered _ = return

entry :: Event -> Stateful
entry (EvKey KEnter _) = searchEntered
entry (EvKey KBS _) = searchBS
entry (EvKey (KChar char) _) = searchChar char
entry _ = return

event :: Event -> Stateful
event e s = case mode s of
    Search ent _ -> (if ent then entry else entered) e s
    _ -> return s
