{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module IO.KeyboardTest
    ( test_keyboard
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((.~))

import Data.Taskell.Lists.Internal (initial)
import Events.Actions.Types
import Events.State                (create, quit)
import Events.State.Types          (State, Stateful, mode)
import Events.State.Types.Mode     (Mode (Shutdown))
import Graphics.Vty.Input.Events   (Event (..), Key (..))
import IO.Keyboard                 (generate)
import IO.Keyboard.Types

tester :: BoundActions -> Event -> Stateful
tester actions ev state = lookup ev actions >>= ($ state)

cleanState :: State
cleanState = create "taskell.md" initial

basicBindings :: Bindings
basicBindings = [(BChar 'q', AQuit)]

basicActions :: Actions
basicActions = [(AQuit, quit)]

basicResult :: Maybe State
basicResult = Just $ (mode .~ Shutdown) cleanState

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
