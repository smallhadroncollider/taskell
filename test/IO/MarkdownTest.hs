{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.MarkdownTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import IO.Markdown.Internal (start)
import IO.Config (MarkdownConfig(..), defaultMarkdownConfig)
import Data.Taskell.Lists (newList, appendToLast)
import Data.Taskell.Task (new, subTask, addSubTask)

alternativeMarkdownConfig :: MarkdownConfig
alternativeMarkdownConfig = MarkdownConfig {
    titleOutput = "##",
    taskOutput = "###",
    subtaskOutput = "-"
}

test_markdown :: TestTree
test_markdown =
    testGroup "IO.Markdown" [
        testGroup "Parsing" [
            testGroup "Default" [
                testCase "List Title" (
                    assertEqual
                        "One list"
                        (newList "Test" empty, [])
                        (start defaultMarkdownConfig (empty, []) ("## Test", 1))
                )

              , testCase "Blank line" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start defaultMarkdownConfig (empty, []) ("", 1))
                )

              , testCase "Just spaces" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start defaultMarkdownConfig (empty, []) ("        ", 1))
                )

              , testCase "Error" (
                    assertEqual
                        "Error on line 1"
                        (empty, [1])
                        (start defaultMarkdownConfig (empty, []) ("Test", 1))
                )

              , testCase "List item" (
                    assertEqual
                        "List item"
                        (appendToLast (new "Test Item") (newList "Test" empty), [])
                        (start defaultMarkdownConfig (newList "Test" empty, []) ("- Test Item", 1))
                )

              , testCase "Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (appendToLast (addSubTask (subTask "Blah" False) (new "Test Item")) (newList "Test" empty), [])
                        (start defaultMarkdownConfig (appendToLast (new "Test Item") (newList "Test" empty), []) ("    * Blah", 1))
                )

              , testCase "Complete Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (appendToLast (addSubTask (subTask "Blah" True) (new "Test Item")) (newList "Test" empty), [])
                        (start defaultMarkdownConfig (appendToLast (new "Test Item") (newList "Test" empty), []) ("    * ~Blah~", 1))
                )

              -- test if list item but no list
              -- test if sub task but no list item
            ]

          , testGroup "Alternative" [
                testCase "List Title" (
                    assertEqual
                        "One list"
                        (newList "Test" empty, [])
                        (start alternativeMarkdownConfig (empty, []) ("## Test", 1))
                )

              , testCase "Blank line" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start alternativeMarkdownConfig (empty, []) ("", 1))
                )

              , testCase "Just spaces" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start alternativeMarkdownConfig (empty, []) ("        ", 1))
                )

              , testCase "Error" (
                    assertEqual
                        "Error on line 1"
                        (empty, [1])
                        (start alternativeMarkdownConfig (empty, []) ("* Test", 1))
                )

              , testCase "List item" (
                    assertEqual
                        "List item"
                        (appendToLast (new "Test Item") (newList "Test" empty), [])
                        (start alternativeMarkdownConfig (newList "Test" empty, []) ("### Test Item", 1))
                )

              , testCase "Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (appendToLast (addSubTask (subTask "Blah" False) (new "Test Item")) (newList "Test" empty), [])
                        (start alternativeMarkdownConfig (appendToLast (new "Test Item") (newList "Test" empty), []) ("- Blah", 1))
                )

              , testCase "Complete Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (appendToLast (addSubTask (subTask "Blah" True) (new "Test Item")) (newList "Test" empty), [])
                        (start alternativeMarkdownConfig (appendToLast (new "Test Item") (newList "Test" empty), []) ("- ~Blah~", 1))
                )

              -- test if list item but no list
              -- test if sub task but no list item
            ]
        ]
    ]
