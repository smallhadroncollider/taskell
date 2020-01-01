{-# LANGUAGE NoImplicitPrelude #-}

module Data.Taskell.Task
    ( Task
    , Update
    , name
    , description
    , due
    , subtasks
    , blank
    , new
    , create
    , duplicate
    , setDescription
    , appendDescription
    , setDue
    , clearDue
    , getSubtask
    , addSubtask
    , hasSubtasks
    , updateSubtask
    , removeSubtask
    , countSubtasks
    , countCompleteSubtasks
    , contains
    , isBlank
    ) where

import Data.Taskell.Task.Internal
