{-# LANGUAGE OverloadedLists #-}

module Taskell.Events.Actions.Normal
    ( event
    , events
    ) where

import ClassyPrelude hiding (delete)

import Data.Char                         (isDigit)
import Graphics.Vty.Input.Events
import Taskell.Events.Actions.Types      as A (ActionType (..))
import Taskell.Events.State
import Taskell.Events.State.Modal.Detail (editDue, showDetail)
import Taskell.Events.State.Modal.Due    (showDue)
import Taskell.Events.State.Types        (Stateful)
import Taskell.IO.Keyboard.Types         (Actions)

events :: Actions
events
    -- general
 =
    [ (A.Quit, quit)
    , (A.Undo, (write =<<) . undo)
    , (A.Redo, (write =<<) . redo)
    , (A.Search, searchMode)
    , (A.Help, showHelp)
    , (A.Due, showDue)
        -- navigation
    , (A.Previous, previous)
    , (A.Next, next)
    , (A.Left, left)
    , (A.Right, right)
    , (A.Bottom, bottom)
    , (A.Top, top)
    -- new tasks
    , (A.New, (startCreate =<<) . (newItem =<<) . store)
    , (A.NewAbove, (startCreate =<<) . (above =<<) . store)
    , (A.NewBelow, (startCreate =<<) . (below =<<) . store)
    , (A.Duplicate, (next =<<) . (write =<<) . (duplicate =<<) . store)
    -- editing tasks
    , (A.Edit, (startEdit =<<) . store)
    , (A.Clear, (startEdit =<<) . (clearItem =<<) . store)
    , (A.Delete, (write =<<) . (delete =<<) . store)
    , (A.Detail, showDetail)
    , (A.DueDate, (editDue =<<) . (store =<<) . showDetail)
    , (A.ClearDate, (write =<<) . (clearDate =<<) . store)
    -- moving tasks
    , (A.MoveUp, (write =<<) . (up =<<) . store)
    , (A.MoveDown, (write =<<) . (down =<<) . store)
    , (A.MoveLeftTop, (write =<<) . (top =<<) . (left =<<) . (moveLeftTop =<<) . store)
    , (A.MoveRightTop, (write =<<) . (top =<<) . (right =<<) . (moveRightTop =<<) . store)
    , (A.MoveLeftBottom, (write =<<) . (bottom =<<) . (left =<<) . (moveLeftBottom =<<) . store)
    , (A.MoveRightBottom, (write =<<) . (bottom =<<) . (right =<<) . (moveRightBottom =<<) . store)
    , (A.Complete, (write =<<) . (moveToLast =<<) . (clearDate =<<) . store)
    , (A.CompleteToTop, (write =<<) . (moveToLastTop =<<) . (clearDate =<<) . store)
    , (A.MoveMenu, showMoveTo)
    -- lists
    , (A.ListNew, (createListStart =<<) . store)
    , (A.ListEdit, (editListStart =<<) . store)
    , (A.ListDelete, (write =<<) . (deleteCurrentList =<<) . store)
    , (A.ListRight, (write =<<) . (listRight =<<) . store)
    , (A.ListLeft, (write =<<) . (listLeft =<<) . store)
    ]

-- Normal
event :: Event -> Stateful
-- selecting lists
event (EvKey (KChar n) _)
    | isDigit n = selectList n
    | otherwise = pure
event (EvKey KEsc _) = clearSearch
-- fallback
event _ = pure
