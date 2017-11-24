module Flow.Actions.Insert (insert) where

import Graphics.Vty.Input.Events
import Flow.State

insert :: Event -> Stateful
insert (EvKey KEnter _) = finishInsert
insert (EvKey KEsc _) = finishInsert
insert (EvKey KBS _) = insertBS
insert (EvKey (KChar char) _) = insertCurrent char
insert _ = id
