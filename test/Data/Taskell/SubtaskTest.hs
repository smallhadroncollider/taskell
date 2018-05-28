{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.SubtaskTest (
    test_subtask
) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.Taskell.Subtask

-- tests
test_subtask :: TestTree
test_subtask =
    testGroup "Data.Taskell.Subtask" [
        testCase "blank" (
            assertEqual
                "creates an empty sub-task"
                (Subtask "" False)
                blank
        )

      , testGroup "new" [
            testCase "completed" (
                assertEqual
                    "creates a new sub-task"
                    (Subtask "Blah" True)
                    (new "Blah" True)
            )

          , testCase "not completed" (
                assertEqual
                    "creates a new sub-task"
                    (Subtask "Blah" False)
                    (new "Blah" False)
            )
        ]

      , testGroup "toggle" [
            testCase "false to true" (
                assertEqual
                    "Sets completed to True"
                    (Subtask "Blah" True)
                    (toggle $ new "Blah" False)
            )

          , testCase "true to false" (
                assertEqual
                    "Sets completed to False"
                    (Subtask "Blah" False)
                    (toggle $ new "Blah" True)
            )
        ]
    ]
