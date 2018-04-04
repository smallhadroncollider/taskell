{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.MarkdownTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (ignoreTest)

import IO.Markdown.Internal (start, parse)
import IO.Config (MarkdownConfig(..), defaultMarkdownConfig, defaultConfig)
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
        testGroup "Line Parsing" [
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
                        (
                            start
                                defaultMarkdownConfig
                                (appendToLast (new "Test Item") (newList "Test" empty), [])
                                ("    * Blah", 1)
                        )
                )

              , testCase "Complete Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (appendToLast (addSubTask (subTask "Blah" True) (new "Test Item")) (newList "Test" empty), [])
                        (start defaultMarkdownConfig (appendToLast (new "Test Item") (newList "Test" empty), []) ("    * ~Blah~", 1))
                )

              , ignoreTest $ testCase "List item without list" (
                    assertEqual
                        "Parse Error"
                        (empty, [1])
                        (start defaultMarkdownConfig (empty, []) ("- Test Item", 1))
                )

              , ignoreTest $ testCase "Sub task without list item" (
                    assertEqual
                        "Parse Error"
                        (newList "Test" empty, [1])
                        (start defaultMarkdownConfig (newList "Test" empty, []) ("    * Blah", 1))
                )
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
            ]
        ]

      , testGroup "Parsing" [
            testCase "List Title" (
                assertEqual
                    "One empty list"
                    (Right (newList "Test" empty))
                    (parse defaultConfig (encodeUtf8 "## Test"))
            )

          , testCase "List Items" (
                assertEqual
                    "List with item"
                    (Right (appendToLast (new "Test Item") (newList "Test" empty)))
                    (parse defaultConfig (encodeUtf8 "## Test\n- Test Item"))
            )

          , testCase "Parsing Errors" (
                assertEqual
                    "Errors"
                    (Left "could not parse line(s) 3, 5")
                    (parse defaultConfig (encodeUtf8 "## Test\n- Test Item\n* Spoon\n- Test Item\nCow"))
            )
        ]
    ]
