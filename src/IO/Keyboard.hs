{-# LANGUAGE NoImplicitPrelude #-}

module IO.Keyboard
    ( generate
    ) where

import ClassyPrelude

import Data.Bitraversable (bisequence)

import IO.Keyboard.Types

generate :: Bindings -> Actions -> BoundActions
generate bindings actions =
    mapFromList . catMaybes $ bisequence <$> (bimap bindingToEvent (`lookup` actions) <$> bindings)
