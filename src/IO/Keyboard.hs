{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Keyboard
    ( generate
    , defaultBindings
    ) where

import ClassyPrelude

import Data.Bitraversable (bitraverse)

import Events.Actions.Types
import IO.Keyboard.Types

generate :: Bindings -> Actions -> BoundActions
generate bindings actions =
    mapFromList . catMaybes $ bitraverse bindingToEvent (`lookup` actions) <$> bindings

defaultBindings :: Bindings
defaultBindings =
    [ (BChar 'q', AQuit)
    , (BChar 'u', AUndo)
    , (BChar '/', ASearch)
    , (BChar '?', AHelp)
    , (BChar 'k', APrevious)
    , (BChar 'j', ANext)
    , (BChar 'h', ALeft)
    , (BChar 'l', ARight)
    , (BChar 'g', ABottom)
    , (BChar 'a', ANew)
    , (BChar 'O', ANewAbove)
    , (BChar 'o', ANewBelow)
    , (BChar 'i', AEdit)
    , (BChar 'C', AClear)
    , (BChar 'D', ADelete)
    , (BKey "Enter", ADetail)
    , (BChar '@', ADueDate)
    , (BChar 'K', AMoveUp)
    , (BChar 'J', AMoveDown)
    , (BChar 'H', AMoveLeft)
    , (BChar 'L', AMoveRight)
    , (BKey "Space", AMoveRight)
    , (BChar 'm', AMoveMenu)
    , (BChar 'N', AListNew)
    , (BChar 'E', AListEdit)
    , (BChar 'X', AListDelete)
    , (BChar '>', AListRight)
    , (BChar '<', AListLeft)
    ]
