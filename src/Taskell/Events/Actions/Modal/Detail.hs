{-# LANGUAGE OverloadedLists #-}

module Taskell.Events.Actions.Modal.Detail
    ( event
    , events
    ) where

import ClassyPrelude

import Graphics.Vty.Input.Events
import Taskell.Events.Actions.Types as A (ActionType(..))
import Taskell.Events.State (clearDate, normalMode, quit, store, undo, write)
import Taskell.Events.State.Modal.Detail as Detail
import Taskell.Events.State.Types
import Taskell.Events.State.Types.Mode (DetailItem(..), DetailMode(..))
import Taskell.IO.Keyboard.Types (Actions)
import qualified Taskell.UI.Draw.Field as F (event)

events :: Actions
events
    -- general
 =
    [ (A.Quit, quit)
    , (A.Undo, (write =<<) . undo)
    , (A.Previous, previousSubtask)
    , (A.Next, nextSubtask)
    , (A.MoveUp, (write =<<) . (up =<<) . store)
    , (A.MoveDown, (write =<<) . (down =<<) . store)
    , (A.New, (Detail.insertMode =<<) . (Detail.lastSubtask =<<) . (Detail.newItem =<<) . store)
    , (A.NewAbove, Detail.newAbove)
    , (A.NewBelow, Detail.newBelow)
    , (A.Edit, (Detail.insertMode =<<) . store)
    , (A.Complete, (write =<<) . (setComplete =<<) . store)
    , (A.Delete, (write =<<) . (Detail.remove =<<) . store)
    , (A.DueDate, (editDue =<<) . store)
    , (A.ClearDate, (write =<<) . (clearDate =<<) . store)
    , (A.Detail, (editDescription =<<) . store)
    ]

normal :: Event -> Stateful
normal (EvKey KEsc _) = normalMode
normal _ = pure

insert :: Event -> Stateful
insert (EvKey KEsc _) s = do
    item <- getCurrentItem s
    case item of
        DetailDescription -> (write =<<) $ finishDescription s
        DetailDate -> showDetail s
        (DetailItem _) -> (write =<<) . (showDetail =<<) $ finishSubtask s
insert (EvKey KEnter _) s = do
    item <- getCurrentItem s
    case item of
        DetailDescription -> (write =<<) $ finishDescription s
        DetailDate -> (write =<<) $ finishDue s
        (DetailItem _) -> (Detail.newBelow =<<) . (write =<<) $ finishSubtask s
insert e s = updateField (F.event e) s

event :: Event -> Stateful
event e s = do
    m <- getCurrentMode s
    case m of
        DetailNormal -> normal e s
        (DetailInsert _) -> insert e s
