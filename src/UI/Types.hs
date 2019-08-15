{-# LANGUAGE NoImplicitPrelude #-}

module UI.Types where

import ClassyPrelude (Eq, Int, Ord, Show)

import Types (ListIndex, TaskIndex)

data ResourceName
    = RNCursor
    | RNTask (ListIndex, TaskIndex)
    | RNList Int
    | RNLists
    | RNModal
    | RNDue Int
    deriving (Show, Eq, Ord)
