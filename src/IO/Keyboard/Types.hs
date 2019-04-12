{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Keyboard.Types where

import ClassyPrelude hiding ((\\))

import Data.List                 ((\\))
import Data.Map.Strict           (Map)
import Graphics.Vty.Input.Events (Event (..), Key (..))

import qualified Events.Actions.Types as A (ActionType (Nothing), allActions)
import           Events.State.Types   (Stateful)

data Binding
    = BChar Char
    | BKey Text
    deriving (Eq, Ord)

type Bindings = [(Binding, A.ActionType)]

type Actions = Map A.ActionType Stateful

type BoundActions = Map Event Stateful

instance Show Binding where
    show (BChar c)   = singleton c
    show (BKey name) = "<" <> unpack name <> ">"

badMapping :: Bindings -> Either Text Bindings
badMapping bindings =
    if null result
        then Right bindings
        else Left "invalid mapping"
  where
    result = filter ((== A.Nothing) . snd) bindings

missing :: Bindings -> Either Text Bindings
missing bindings =
    if null result
        then Right bindings
        else Left "missing mapping"
  where
    bnd = A.Nothing : (snd <$> bindings)
    result = A.allActions \\ bnd

bindingsToText :: Bindings -> A.ActionType -> [Text]
bindingsToText bindings key = tshow . fst <$> toList (filterMap (== key) bindings)

bindingToEvent :: Binding -> Maybe Event
bindingToEvent (BChar char)       = pure $ EvKey (KChar char) []
bindingToEvent (BKey "Space")     = pure $ EvKey (KChar ' ') []
bindingToEvent (BKey "Backspace") = pure $ EvKey KBS []
bindingToEvent (BKey "Enter")     = pure $ EvKey KEnter []
bindingToEvent (BKey "Left")      = pure $ EvKey KLeft []
bindingToEvent (BKey "Right")     = pure $ EvKey KRight []
bindingToEvent (BKey "Up")        = pure $ EvKey KUp []
bindingToEvent (BKey "Down")      = pure $ EvKey KDown []
bindingToEvent _                  = Nothing
