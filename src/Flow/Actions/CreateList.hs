module Flow.Actions.CreateList (event) where

import Graphics.Vty.Input.Events
import qualified Flow.State as S

event :: Event -> S.Stateful
event (EvKey KEnter _) = S.createListFinish
event (EvKey KEsc _) = S.createListCancel
event (EvKey KBS _) = S.createListBS
event (EvKey (KChar char) _) = S.createListChar char
event _ = return
