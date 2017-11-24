module Flow.Actions.CreateList (createList) where

import Graphics.Vty.Input.Events
import qualified Flow.State as S

createList :: Event -> S.Stateful
createList (EvKey KEnter _) = S.createListFinish
createList (EvKey KEsc _) = S.createListCancel
createList (EvKey KBS _) = S.createListBS
createList (EvKey (KChar char) _) = S.createListChar char
createList _ = id
