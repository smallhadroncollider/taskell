{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Modal.Detail
    ( event
    , events
    ) where

import ClassyPrelude

import           Events.Actions.Types      (ActionType (..))
import           Events.State
import           Events.State.Modal.Detail as Detail
import           Events.State.Types
import           Events.State.Types.Mode   (DetailItem (..), DetailMode (..))
import           Graphics.Vty.Input.Events
import           IO.Keyboard.Types         (Actions)
import qualified UI.Field                  as F (event)

events :: Actions
events
    -- general
 =
    [ (AQuit, quit)
    , (AUndo, (write =<<) . undo)
    , (APrevious, previousSubtask)
    , (ANext, nextSubtask)
    , (ANew, (Detail.insertMode =<<) . (Detail.lastSubtask =<<) . (Detail.newItem =<<) . store)
    , (AEdit, (Detail.insertMode =<<) . store)
    , (AMoveRight, (write =<<) . (setComplete =<<) . store)
    , (ADelete, (write =<<) . (Detail.remove =<<) . store)
    , (ADueDate, (editDue =<<) . store)
    , (ADetail, (editDescription =<<) . store)
    ]

normal :: Event -> Stateful
normal (EvKey KEsc _) = normalMode
normal _              = pure

insert :: Event -> Stateful
insert (EvKey KEsc _) s = do
    item <- getCurrentItem s
    case item of
        DetailDescription -> (write =<<) $ finishDescription s
        DetailDate        -> (write =<<) $ finishDue s
        (DetailItem _)    -> (write =<<) . (showDetail =<<) $ finishSubtask s
insert (EvKey KEnter _) s = do
    item <- getCurrentItem s
    case item of
        DetailDescription -> (write =<<) $ finishDescription s
        DetailDate -> (write =<<) $ finishDue s
        (DetailItem _) ->
            (Detail.lastSubtask =<<) . (Detail.newItem =<<) . (store =<<) . (write =<<) $
            finishSubtask s
insert e s = updateField (F.event e) s

event :: Event -> Stateful
event e s = do
    m <- getCurrentMode s
    case m of
        DetailNormal     -> normal e s
        (DetailInsert _) -> insert e s
