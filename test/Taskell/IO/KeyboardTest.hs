{-# LANGUAGE OverloadedLists #-}

module Taskell.IO.KeyboardTest
    ( test_keyboard
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((.~))

import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Zones (utcTZ)

import Graphics.Vty.Input.Events       (Event (..), Key (..))
import Taskell.Data.Lists.Internal     (initial)
import Taskell.Events.Actions.Types    as A
import Taskell.Events.State            (create, quit)
import Taskell.Events.State.Types      (State, Stateful, mode)
import Taskell.Events.State.Types.Mode (Mode (Shutdown))
import Taskell.IO.Keyboard             (generate)
import Taskell.IO.Keyboard.Types

mockTime :: UTCTime
mockTime = UTCTime (ModifiedJulianDay 20) (secondsToDiffTime 0)

tester :: BoundActions -> Event -> Stateful
tester actions ev state = lookup ev actions >>= ($ state)

cleanState :: State
cleanState = create utcTZ mockTime "taskell.md" initial

basicBindings :: Bindings
basicBindings = [(BChar 'q', A.Quit)]

basicActions :: Actions
basicActions = [(A.Quit, quit)]

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
