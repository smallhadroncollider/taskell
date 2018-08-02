{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Modal.Detail (event) where

import ClassyPrelude

import Graphics.Vty.Input.Events
import Events.State
import Events.State.Types
import Events.State.Types.Mode (DetailMode(..), DetailItem(..))
import Events.State.Modal.Detail as Detail
import qualified UI.Field as F (event)

normal :: Event -> Stateful
normal (EvKey (KChar 'q') _) = quit
normal (EvKey KEsc _) = normalMode
normal (EvKey KEnter _) = (editDescription =<<) . store
normal (EvKey (KChar ' ') _) = (write =<<) . (setComplete =<<) . store
normal (EvKey (KChar 'k') _) = previousSubtask
normal (EvKey (KChar 'j') _) = nextSubtask
normal (EvKey (KChar 'a') _) = (Detail.insertMode =<<) . (Detail.lastSubtask =<<) . (Detail.newItem =<<) . store
normal (EvKey (KChar 'e') _) = (Detail.insertMode =<<) . store
normal (EvKey (KChar 'D') _) = (write =<<) . (Detail.remove =<<) . store
normal (EvKey (KChar 'u') _) = (write =<<) . undo
normal (EvKey (KChar '@') _) = (editDue =<<) . store
normal _ = return

insert :: Event -> Stateful
insert (EvKey KEsc _) s = do
    item <- getCurrentItem s
    case item of
        DetailDescription -> (write =<<) $ finishDescription s
        DetailDate -> (write =<<) $ finishDue s
        (DetailItem _) -> (write =<<) . (showDetail =<<) $ finishSubtask s

insert (EvKey KEnter _) s = do
    item <- getCurrentItem s
    case item of
        DetailDescription -> (write =<<) $ finishDescription s
        DetailDate -> (write =<<) $ finishDue s
        (DetailItem _) -> (Detail.lastSubtask =<<) . (Detail.newItem =<<) . (store =<<) . (write =<<) $ finishSubtask s

insert e s = updateField (F.event e) s

event :: Event -> Stateful
event e s = do
    m <- getCurrentMode s
    case m of
        DetailNormal -> normal e s
        (DetailInsert _) -> insert e s
