module UI.Types where

data ResourceName =
      RNTask (Int, Int)
    | RNList Int
    | RNLists
    | RNSearch
    | RNModal
    | RNModalItem Int
    deriving (Show, Eq, Ord)
