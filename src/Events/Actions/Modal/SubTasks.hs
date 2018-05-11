{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Modal.SubTasks (event) where

import ClassyPrelude

import Graphics.Vty.Input.Events
import Events.State
import Events.State.Types
import Events.State.Modal.SubTasks as ST
import qualified UI.Field as F (event)

normal :: Event -> Stateful
normal (EvKey (KChar 'q') _) = quit
normal (EvKey KEsc _) = normalMode
normal (EvKey KEnter _) = (editSummary =<<) . store
normal (EvKey (KChar ' ') _) = (write =<<) . (setComplete =<<) . store
normal (EvKey (KChar 'k') _) = previousSubTask
normal (EvKey (KChar 'j') _) = nextSubTask
normal (EvKey (KChar 'a') _) = (ST.insertMode =<<) . (ST.lastSubTask =<<) . (ST.newItem =<<) . store
normal (EvKey (KChar 'e') _) = (ST.insertMode =<<) . store
normal (EvKey (KChar 'D') _) = (write =<<) . (ST.remove =<<) . store
normal (EvKey (KChar 'u') _) = (write =<<) . undo
normal _ = return

insert :: Event -> Stateful
insert (EvKey KEsc _) s = (write =<<) . (showSubTasks =<<) $ finishSubTask s
insert (EvKey KEnter _) s = case mode s of
    Modal (SubTasks (-1) _) -> (write =<<) $ finishSummary s
    Modal (SubTasks _ _) -> (ST.lastSubTask =<<) . (ST.newItem =<<) . (store =<<) . (write =<<) $ finishSubTask s
    _ -> return s
insert e s = return $ case mode s of
    Modal (SubTasks i (STInsert field)) -> s {
        mode = Modal (SubTasks i (STInsert (F.event e field)))
    }
    _ -> s

event :: Event -> Stateful
event e s = case mode s of
    Modal (SubTasks _ STNormal) -> normal e s
    Modal (SubTasks _ (STInsert _)) -> insert e s
    _ -> return s
