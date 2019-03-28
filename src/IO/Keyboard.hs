{-# LANGUAGE NoImplicitPrelude #-}

module IO.Keyboard
    ( generate
    ) where

import ClassyPrelude

import Data.Bitraversable (bitraverse)

import IO.Keyboard.Types

generate :: Bindings -> Actions -> BoundActions
generate bindings actions =
    mapFromList . catMaybes $ bitraverse bindingToEvent (`lookup` actions) <$> bindings
