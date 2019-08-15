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
    , duplicate
    , setDescription
    , appendDescription
    , setDue
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
