module UI.Types where

import Events.State (Pointer)

data ResourceName =
      RNTask Pointer
    | RNList Int
    | RNLists
    deriving (Show, Eq, Ord)
