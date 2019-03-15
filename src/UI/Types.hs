{-# LANGUAGE NoImplicitPrelude #-}

module UI.Types where

import ClassyPrelude (Eq, Int, Ord, Show)

data ResourceName
    = RNCursor
    | RNTask (Int, Int)
    | RNList Int
    | RNLists
    | RNModal
    deriving (Show, Eq, Ord)
