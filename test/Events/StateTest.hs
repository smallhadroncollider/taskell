{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Events.StateTest (
    test_state
) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (ignoreTest)

import Events.State
import Events.State.Types
import Data.Taskell.Lists

testState :: State
testState = State {
    mode = Normal,
    lists = empty,
    history = [],
    current = (0, 0),
    path = "test.md",
    io = Nothing
}

-- tests
test_state :: TestTree
test_state =
    testGroup "Events.State" [
        testCase "quit" (
            assertEqual
                "Retuns Just with mode set to Shutdown"
                (Just (testState { mode = Shutdown }))
                (quit testState)
        )

      , testCase "continue" (
            assertEqual
                "Retuns with io set to "
                (testState { io = Nothing })
                (continue testState)
        )
    ]
