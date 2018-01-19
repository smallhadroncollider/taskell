module UI.Types where

import Flow.State (Pointer)

data ResourceName =
      RNTask Pointer
    | RNList Int
    | RNLists
    deriving (Show, Eq, Ord)
