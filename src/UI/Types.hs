module UI.Types where

import Events.State (Pointer)

data ResourceName =
      RNTask Pointer
    | RNList Int
    | RNLists
    | RNModal
    deriving (Show, Eq, Ord)
