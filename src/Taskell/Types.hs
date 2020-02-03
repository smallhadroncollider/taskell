{-# LANGUAGE NoImplicitPrelude #-}

module Taskell.Types where

import ClassyPrelude

newtype ListIndex = ListIndex
    { showListIndex :: Int
    } deriving (Show, Eq, Ord)

newtype TaskIndex = TaskIndex
    { showTaskIndex :: Int
    } deriving (Show, Eq, Ord)

type Pointer = (ListIndex, TaskIndex)

startPointer :: Pointer
startPointer = (ListIndex 0, TaskIndex 0)
