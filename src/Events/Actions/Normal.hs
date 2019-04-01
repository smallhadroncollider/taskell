{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Events.Actions.Normal
    ( event
    , events
    ) where

import ClassyPrelude hiding (delete)

import Data.Char                 (isDigit)
import Data.Map.Strict           (Map)
import Events.State
import Events.State.Modal.Detail (editDue, showDetail)
import Events.State.Types        (Stateful)
import Graphics.Vty.Input.Events

events :: Map Text Stateful
events
    -- general
 =
    [ ("quit", quit)
    , ("undo", (write =<<) . undo)
    , ("search", searchMode)
    , ("help", showHelp)
        -- navigation
    , ("previous", previous)
    , ("next", next)
    , ("left", left)
    , ("right", right)
    , ("bottom", bottom)
    -- new tasks
    , ("new", (startCreate =<<) . (newItem =<<) . store)
    , ("newAbove", (startCreate =<<) . (above =<<) . store)
    , ("newBelow", (startCreate =<<) . (below =<<) . store)
    -- editing tasks
    , ("edit", (startEdit =<<) . store)
    , ("clear", (startEdit =<<) . (clearItem =<<) . store)
    , ("delete", (write =<<) . (delete =<<) . store)
    , ("detail", showDetail)
    , ("dueDate", (editDue =<<) . (store =<<) . showDetail)
    -- moving tasks
    , ("moveUp", (write =<<) . (up =<<) . store)
    , ("moveDown", (write =<<) . (down =<<) . store)
    , ("moveLeft", (write =<<) . (bottom =<<) . (left =<<) . (moveLeft =<<) . store)
    , ("moveRight", (write =<<) . (bottom =<<) . (right =<<) . (moveRight =<<) . store)
    , ("moveMenu", showMoveTo)
    -- lists
    , ("listNew", (createListStart =<<) . store)
    , ("listEdit", (editListStart =<<) . store)
    , ("listDelete", (write =<<) . (deleteCurrentList =<<) . store)
    , ("listRight", (write =<<) . (listRight =<<) . store)
    , ("listLeft", (write =<<) . (listLeft =<<) . store)
    ]

-- Normal
event :: Event -> Stateful
-- selecting lists
event (EvKey (KChar n) _)
    | isDigit n = selectList n
    | otherwise = pure
-- fallback
event _ = pure
