{-# LANGUAGE NoImplicitPrelude #-}

module UI.Types where

import ClassyPrelude (Eq, Int, Ord, Show)

newtype ListIndex = ListIndex
    { showListIndex :: Int
    } deriving (Show, Eq, Ord)

newtype TaskIndex = TaskIndex
    { showTaskIndex :: Int
    } deriving (Show, Eq, Ord)

data ResourceName
    = RNCursor
    | RNTask (ListIndex, TaskIndex)
    | RNList Int
    | RNLists
    | RNModal
    | RNDue Int
    deriving (Show, Eq, Ord)
