{-# LANGUAGE NoImplicitPrelude #-}
module UI.Types where

import ClassyPrelude (Int, Show, Eq, Ord)

data ResourceName =
      RNCursor
    | RNTask (Int, Int)
    | RNList Int
    | RNLists
    | RNSearch
    | RNModal
    | RNModalItem Int
    deriving (Show, Eq, Ord)
