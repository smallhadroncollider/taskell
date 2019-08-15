{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import ClassyPrelude

newtype ListIndex = ListIndex
    { showListIndex :: Int
    } deriving (Show, Eq, Ord)

newtype TaskIndex = TaskIndex
    { showTaskIndex :: Int
    } deriving (Show, Eq, Ord)

type Pointer = (ListIndex, TaskIndex)
