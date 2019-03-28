{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module IO.KeyboardTest
    ( test_keyboard
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((.~))

import Events.State              (create, quit)
import Events.State.Types        (State, Stateful, mode)
import Events.State.Types.Mode   (Mode (Shutdown))
import Graphics.Vty.Input.Events (Event (..), Key (..))
import IO.Keyboard               (generate)
import IO.Keyboard.Types

cleanState :: State
cleanState = create "taskell.md" []

basicBindings :: Bindings
basicBindings = [(BChar 'q', "quit")]

basicActions :: Actions
basicActions = [("quit", quit)]

basicResult :: Maybe State
basicResult = Just $ (mode .~ Shutdown) cleanState

tester :: BoundActions -> Event -> Stateful
tester bactions ev state = do
    fn <- lookup ev bactions
    fn state

test_keyboard :: TestTree
test_keyboard =
    testGroup
        "IO.Keyboard"
        [ testCase
              "generate"
              (assertEqual
                   "Parses basic"
                   basicResult
                   (tester (generate basicBindings basicActions) (EvKey (KChar 'q') []) cleanState))
        ]
