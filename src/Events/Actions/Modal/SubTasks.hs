module Events.Actions.Modal.SubTasks (event) where

import Graphics.Vty.Input.Events
import Events.State
import Events.State.Types
import Events.State.Modal.SubTasks as ST

normal :: Event -> Stateful
normal (EvKey (KChar 'q') _) = quit
normal (EvKey KEsc _) = normalMode
normal (EvKey KEnter _) = normalMode
normal (EvKey (KChar ' ') _) = (write =<<) . (setComplete =<<) . store
normal (EvKey (KChar 'k') _) = previousSubTask
normal (EvKey (KChar 'j') _) = nextSubTask
normal (EvKey (KChar 'a') _) = (ST.insertMode =<<) . (ST.lastSubTask =<<) . (ST.newItem =<<) . store
normal (EvKey (KChar 'e') _) = (ST.insertMode =<<) . store
normal (EvKey (KChar 'D') _) = (write =<<) . (ST.remove =<<) . store
normal _ = return

insert :: Event -> Stateful
insert (EvKey KEsc _) = (write =<<) . showSubTasks
insert (EvKey KBS _) = ST.insertBS
insert (EvKey (KChar char) _) = ST.insertCurrent char
insert _ = return

event :: Event -> Stateful
event e s = case mode s of
    Modal (SubTasks _ STNormal) -> normal e s
    Modal (SubTasks _ STInsert) -> insert e s
    _ -> return s
