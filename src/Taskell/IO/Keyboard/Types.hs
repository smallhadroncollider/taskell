{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.IO.Keyboard.Types where

import ClassyPrelude

import Data.Map.Strict           (Map)
import Graphics.Vty.Input.Events (Event (..), Key (..))

import qualified Taskell.Events.Actions.Types as A (ActionType)
import           Taskell.Events.State.Types   (Stateful)

data Binding
    = BChar Char
    | BKey Text
    deriving (Eq, Ord)

type Bindings = [(Binding, A.ActionType)]

type Actions = Map A.ActionType Stateful

type BoundActions = Map Event Stateful

instance Show Binding where
    show (BChar c)      = singleton c
    show (BKey "Up")    = "↑"
    show (BKey "Down")  = "↓"
    show (BKey "Left")  = "←"
    show (BKey "Right") = "→"
    show (BKey name)    = "<" <> unpack name <> ">"

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
