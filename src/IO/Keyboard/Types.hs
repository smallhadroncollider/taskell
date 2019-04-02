{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Keyboard.Types where

import ClassyPrelude hiding ((\\))

import Data.List                 ((\\))
import Data.Map.Strict           (Map)
import Graphics.Vty.Input.Events (Event (..), Key (..))

import Events.Actions.Types (ActionType (ANothing), allActions)
import Events.State.Types   (Stateful)

data Binding
    = BChar Char
    | BKey Text
    deriving (Eq, Ord)

type Bindings = [(Binding, ActionType)]

type Actions = Map ActionType Stateful

type BoundActions = Map Event Stateful

instance Show Binding where
    show (BChar c)   = singleton c
    show (BKey name) = "<" <> unpack name <> ">"

badMapping :: Bindings -> Maybe [Binding]
badMapping bindings =
    if null result
        then Nothing
        else Just (fst <$> result)
  where
    result = filter ((== ANothing) . snd) bindings

missing :: Bindings -> Maybe [ActionType]
missing bindings =
    if null result
        then Nothing
        else Just result
  where
    bnd = ANothing : (snd <$> bindings)
    result = allActions \\ bnd

bindingsToText :: Bindings -> ActionType -> [Text]
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
