{-# LANGUAGE NoImplicitPrelude #-}

module IO.Keyboard.Types where

import ClassyPrelude

import Data.Map.Strict (Map)

data Binding
    = BChar Char
    | BKey Text
    deriving (Eq, Ord, Show)

type Bindings = Map Binding Text
