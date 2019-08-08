{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Keyboard
    ( generate
    , defaultBindings
    , badMapping
    , addMissing
    ) where

import ClassyPrelude hiding ((\\))

import Data.Bitraversable (bitraverse)
import Data.List          ((\\))

import qualified Events.Actions.Types as A
import           IO.Keyboard.Types

generate :: Bindings -> Actions -> BoundActions
generate bindings actions =
    mapFromList . catMaybes $ bitraverse bindingToEvent (`lookup` actions) <$> bindings

badMapping :: Bindings -> Either Text Bindings
badMapping bindings =
    if null result
        then Right bindings
        else Left "invalid mapping"
  where
    result = filter ((== A.Nothing) . snd) bindings

addMissing :: Bindings -> Bindings
addMissing bindings = bindings <> replaced
  where
    bnd = A.Nothing : (snd <$> bindings)
    result = A.allActions \\ bnd
    replaced = concat $ replace <$> result

replace :: A.ActionType -> Bindings
replace action = filter ((==) action . snd) defaultBindings

defaultBindings :: Bindings
defaultBindings =
    [ (BChar 'q', A.Quit)
    , (BChar 'u', A.Undo)
    , (BChar '/', A.Search)
    , (BChar '?', A.Help)
    , (BChar 'k', A.Previous)
    , (BChar 'j', A.Next)
    , (BChar 'h', A.Left)
    , (BChar 'l', A.Right)
    , (BChar 'g', A.Bottom)
    , (BChar 'a', A.New)
    , (BChar 'O', A.NewAbove)
    , (BChar 'o', A.NewBelow)
    , (BChar '+', A.Duplicate)
    , (BChar 'e', A.Edit)
    , (BChar 'A', A.Edit)
    , (BChar 'i', A.Edit)
    , (BChar 'C', A.Clear)
    , (BChar 'D', A.Delete)
    , (BKey "Enter", A.Detail)
    , (BChar '@', A.DueDate)
    , (BChar 'K', A.MoveUp)
    , (BChar 'J', A.MoveDown)
    , (BChar 'H', A.MoveLeft)
    , (BChar 'L', A.MoveRight)
    , (BKey "Space", A.MoveRight)
    , (BChar 'm', A.MoveMenu)
    , (BChar 'N', A.ListNew)
    , (BChar 'E', A.ListEdit)
    , (BChar 'X', A.ListDelete)
    , (BChar '>', A.ListRight)
    , (BChar '<', A.ListLeft)
    ]
