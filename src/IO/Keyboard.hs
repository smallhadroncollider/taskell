{-# LANGUAGE NoImplicitPrelude #-}

module IO.Keyboard
    ( generate
    ) where

import ClassyPrelude

import IO.Keyboard.Types

noMaybes :: (Maybe a, Maybe b) -> Maybe (a, b)
noMaybes (Just a, Just b) = Just (a, b)
noMaybes _                = Nothing

generate :: Bindings -> Actions -> BoundActions
generate bindings actions = mapFromList . catMaybes $ noMaybes <$> swp
  where
    swp = bimap bindingToEvent (`lookup` actions) <$> bindings
