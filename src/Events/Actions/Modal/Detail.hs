{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Modal.Detail
    ( event
    , events
    ) where

import ClassyPrelude

import           Events.State
import           Events.State.Modal.Detail as Detail
import           Events.State.Types
import           Events.State.Types.Mode   (DetailItem (..), DetailMode (..))
import           Graphics.Vty.Input.Events
import qualified UI.Field                  as F (event)

events :: Map Text Stateful
events
    -- general
 =
    [ ("quit", quit)
    , ("undo", (write =<<) . undo)
    , ("previous", previousSubtask)
    , ("next", nextSubtask)
    , ("new", (Detail.insertMode =<<) . (Detail.lastSubtask =<<) . (Detail.newItem =<<) . store)
    , ("edit", (Detail.insertMode =<<) . store)
    , ("moveRight", (write =<<) . (setComplete =<<) . store)
    , ("delete", (write =<<) . (Detail.remove =<<) . store)
    , ("dueDate", (editDue =<<) . store)
    ]

normal :: Event -> Stateful
normal (EvKey KEsc _)   = normalMode
normal (EvKey KEnter _) = (editDescription =<<) . store
normal _                = pure

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
