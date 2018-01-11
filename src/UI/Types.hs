module UI.Types where

import Flow.State (Pointer)

data ResourceName = ListLocation Pointer | ViewportName Int | MainView deriving (Show, Eq, Ord)
