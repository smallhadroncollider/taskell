module UI.Types where

import Events.State.Types (Pointer)

data ResourceName =
      RNTask Pointer
    | RNList Int
    | RNLists
    | RNModal
    | RNModalItem Int
    deriving (Show, Eq, Ord)
