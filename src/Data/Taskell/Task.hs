{-# LANGUAGE NoImplicitPrelude #-}
module Data.Taskell.Task (
    Task
  , name
  , description
  , due
  , subtasks
  , blank
  , new
  , setDescription
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
