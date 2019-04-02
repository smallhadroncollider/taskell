{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Normal
    ( event
    , events
    ) where

import ClassyPrelude hiding (delete)

import Data.Char                 (isDigit)
import Events.Actions.Types      (ActionType (..))
import Events.State
import Events.State.Modal.Detail (editDue, showDetail)
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events
import IO.Keyboard.Types         (Actions)

events :: Actions
events
    -- general
 =
    [ (AQuit, quit)
    , (AUndo, (write =<<) . undo)
    , (ASearch, searchMode)
    , (AHelp, showHelp)
        -- navigation
    , (APrevious, previous)
    , (ANext, next)
    , (ALeft, left)
    , (ARight, right)
    , (ABottom, bottom)
    -- new tasks
    , (ANew, (startCreate =<<) . (newItem =<<) . store)
    , (ANewAbove, (startCreate =<<) . (above =<<) . store)
    , (ANewBelow, (startCreate =<<) . (below =<<) . store)
    -- editing tasks
    , (AEdit, (startEdit =<<) . store)
    , (AClear, (startEdit =<<) . (clearItem =<<) . store)
    , (ADelete, (write =<<) . (delete =<<) . store)
    , (ADetail, showDetail)
    , (ADueDate, (editDue =<<) . (store =<<) . showDetail)
    -- moving tasks
    , (AMoveUp, (write =<<) . (up =<<) . store)
    , (AMoveDown, (write =<<) . (down =<<) . store)
    , (AMoveLeft, (write =<<) . (bottom =<<) . (left =<<) . (moveLeft =<<) . store)
    , (AMoveRight, (write =<<) . (bottom =<<) . (right =<<) . (moveRight =<<) . store)
    , (AMoveMenu, showMoveTo)
    -- lists
    , (AListNew, (createListStart =<<) . store)
    , (AListEdit, (editListStart =<<) . store)
    , (AListDelete, (write =<<) . (deleteCurrentList =<<) . store)
    , (AListRight, (write =<<) . (listRight =<<) . store)
    , (AListLeft, (write =<<) . (listLeft =<<) . store)
    ]

-- Normal
event :: Event -> Stateful
-- selecting lists
event (EvKey (KChar n) _)
    | isDigit n = selectList n
    | otherwise = pure
-- fallback
event _ = pure
