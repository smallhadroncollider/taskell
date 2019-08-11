{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Modal.Detail
    ( event
    , events
    ) where

import ClassyPrelude
import Control.Lens  ((^.))

import           Events.Actions.Types      as A (ActionType (..))
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
    [ (A.Quit, quit)
    , (A.Undo, (write =<<) . undo)
    , (A.Previous, previousSubtask)
    , (A.Next, nextSubtask)
    , (A.New, (Detail.insertMode =<<) . (Detail.lastSubtask =<<) . (Detail.newItem =<<) . store)
    , (A.Edit, (Detail.insertMode =<<) . store)
    , (A.MoveRight, (write =<<) . (setComplete =<<) . store)
    , (A.Delete, (write =<<) . (Detail.remove =<<) . store)
    , (A.DueDate, (editDue =<<) . store)
    , (A.Detail, (editDescription =<<) . store)
    ]

normal :: Event -> Stateful
normal (EvKey KEsc _) = normalMode
normal _              = pure

insert :: Event -> Stateful
insert (EvKey KEsc _) s = do
    item <- getCurrentItem (s ^. mode)
    case item of
        DetailDescription -> (write =<<) $ finishDescription s
        DetailDate        -> (write =<<) $ finishDue s
        (DetailItem _)    -> (write =<<) . (showDetail =<<) $ finishSubtask s
insert (EvKey KEnter _) s = do
    item <- getCurrentItem (s ^. mode)
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
