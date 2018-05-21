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
import qualified Data.Sequence as S (lookup)
import qualified Data.Taskell.List as L (empty, append)
import qualified Data.Taskell.Task as T (new)

testState :: State
testState = State {
    mode = Normal,
    lists = empty,
    history = [],
    current = (0, 0),
    path = "test.md",
    io = Nothing
}

moveToState :: State
moveToState = State {
    mode = Modal MoveTo,
    lists = fromList [
        L.empty "List 1",
        L.empty "List 2",
        L.empty "List 3",
        L.empty "List 4",
        L.append (L.empty "List 5") (T.new "Test Item"),
        L.empty "List 6",
        L.empty "List 7",
        L.empty "List 8",
        L.empty "List 9"
    ],
    history = [],
    current = (4, 0),
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

        -- 1 - a, 2 - b, 3 - c, 4 - d, 5 - e, 6 - f, 7 - g, 8 - h, 9 - i
      , testGroup "moveTo" [
            testCase "first" (
                assertEqual
                    "Moves to first list"
                    (Just (L.append (L.empty "List 1") (T.new "Test Item")))
                    (S.lookup 0 . lists =<< moveTo 'a' moveToState)
            )

          , testCase "second" (
                assertEqual
                    "Moves to second list"
                    (Just (L.append (L.empty "List 2") (T.new "Test Item")))
                    (S.lookup 1 . lists =<< moveTo 'b' moveToState)
            )

          , testCase "fourth" (
                assertEqual
                    "Moves to fourth list"
                    (Just (L.append (L.empty "List 4") (T.new "Test Item")))
                    (S.lookup 3 . lists =<< moveTo 'd' moveToState)
            )

          , testCase "sixth" (
                assertEqual
                    "Moves to sixth list"
                    (Just (L.append (L.empty "List 6") (T.new "Test Item")))
                    (S.lookup 5 . lists =<< moveTo 'f' moveToState)
            )

          , testCase "penultimate" (
                assertEqual
                    "Moves to penultime list"
                    (Just (L.append (L.empty "List 8") (T.new "Test Item")))
                    (S.lookup 7 . lists =<< moveTo 'h' moveToState)
            )

          , testCase "last" (
                assertEqual
                    "Moves to last list"
                    (Just (L.append (L.empty "List 9") (T.new "Test Item")))
                    (S.lookup 8 . lists =<< moveTo 'i' moveToState)
            )

          , testCase "current list" (
                assertEqual
                    "Doesn't do anything"
                    Nothing
                    (moveTo 'e' moveToState)
            )

          , testCase "invalid option" (
                assertEqual
                    "Doesn't do anything"
                    Nothing
                    (moveTo 'q' moveToState)
            )
        ]
    ]
