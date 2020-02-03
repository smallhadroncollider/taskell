{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.Events.State.HistoryTest
    ( test_history
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Taskell.Events.State.History
import Taskell.Events.State.Types

make :: [Integer] -> Integer -> [Integer] -> History Integer
make = History

testHistory :: History Integer
testHistory = make empty 0 empty

-- tests
test_history :: TestTree
test_history =
    testGroup
        "Events.State.History"
        [ testGroup
              "undo"
              [ testCase "empty" (assertEqual "Nothing changes" testHistory (undo testHistory))
              , testCase
                    "one undo"
                    (assertEqual "Goes back" (make empty 0 [1]) (undo $ make [0] 1 empty))
              , testCase
                    "two undo"
                    (assertEqual
                         "Goes back"
                         (make empty 0 [1, 2])
                         (undo . undo $ make [1, 0] 2 empty))
              , testCase
                    "three undo"
                    (assertEqual
                         "Goes back"
                         (make empty 0 [1, 2, 3])
                         (undo . undo . undo $ make [2, 1, 0] 3 empty))
              ]
        , testGroup
              "redo"
              [ testCase "empty" (assertEqual "Nothing changes" testHistory (redo testHistory))
              , testCase
                    "one redo"
                    (assertEqual "Goes forward" (make [0] 1 empty) (redo $ make empty 0 [1]))
              , testCase
                    "two redo"
                    (assertEqual
                         "Goes forward"
                         (make [1, 0] 2 empty)
                         (redo . redo $ make empty 0 [1, 2]))
              , testCase
                    "three redo"
                    (assertEqual
                         "Goes forward"
                         (make [2, 1, 0] 3 empty)
                         (redo . redo . redo $ make empty 0 [1, 2, 3]))
              ]
        , testGroup
              "mix"
              [ testCase
                    "empty"
                    (assertEqual
                         "Nothing changes"
                         testHistory
                         (undo . redo . undo . redo . undo $ testHistory))
              , testCase
                    "redo undo"
                    (assertEqual
                         "Nothing changes"
                         (make [4, 3, 2, 1, 0] 5 [6, 7, 8, 9, 10])
                         (undo . redo $ make [4, 3, 2, 1, 0] 5 [6, 7, 8, 9, 10]))
              , testCase
                    "undo redo"
                    (assertEqual
                         "Nothing changes"
                         (make [4, 3, 2, 1, 0] 5 [6, 7, 8, 9, 10])
                         (redo . undo $ make [4, 3, 2, 1, 0] 5 [6, 7, 8, 9, 10]))
              ]
        , testGroup
              "store"
              [ testCase
                    "empty"
                    (assertEqual "Stores current value" (make [0] 0 empty) (store testHistory))
              , testCase
                    "undo store redo"
                    (assertEqual
                         "Clears redo"
                         (make [4, 3, 2, 1, 0] 4 empty)
                         (redo . store . undo $ make [4, 3, 2, 1, 0] 5 [6, 7, 8, 9, 10]))
              ]
        ]
