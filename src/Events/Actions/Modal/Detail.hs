{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Modal.Detail (event) where

import ClassyPrelude

import Graphics.Vty.Input.Events
import Events.State
import Events.State.Types
import Events.State.Modal.Detail as Detail
import qualified UI.Field as F (event)

normal :: Event -> Stateful
normal (EvKey (KChar 'q') _) = quit
normal (EvKey KEsc _) = normalMode
normal (EvKey KEnter _) = (editSummary =<<) . store
normal (EvKey (KChar ' ') _) = (write =<<) . (setComplete =<<) . store
normal (EvKey (KChar 'k') _) = previousSubTask
normal (EvKey (KChar 'j') _) = nextSubTask
normal (EvKey (KChar 'a') _) = (Detail.insertMode =<<) . (Detail.lastSubTask =<<) . (Detail.newItem =<<) . store
normal (EvKey (KChar 'e') _) = (Detail.insertMode =<<) . store
normal (EvKey (KChar 'D') _) = (write =<<) . (Detail.remove =<<) . store
normal (EvKey (KChar 'u') _) = (write =<<) . undo
normal _ = return

insert :: Event -> Stateful
insert (EvKey KEsc _) s
    | editingSummary s = (write =<<) $ finishSummary s
    | otherwise = (write =<<) . (showDetail =<<) $ finishSubTask s
insert (EvKey KEnter _) s
    | editingSummary s = (write =<<) $ finishSummary s
    | otherwise = (Detail.lastSubTask =<<) . (Detail.newItem =<<) . (store =<<) . (write =<<) $ finishSubTask s
insert e s = updateField (F.event e) s

event :: Event -> Stateful
event e s = case getCurrentMode s of
    Just DetailNormal -> normal e s
    Just (DetailInsert _) -> insert e s
    _ -> return s
