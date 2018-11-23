{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.TextEditTest (
    test_textEdit
) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.Text ()

import UI.TextEdit

test_textEdit :: TestTree
test_textEdit =
    testGroup "UI.TextEdit" [
        testGroup "Setting then getting" [
            testCase "Basic string" (
                assertEqual
                    "Should return same string"
                    "Blah"
                    (getText $ textEdit "Blah")
            )

          , testCase "New lines" (
                assertEqual
                    "Should return same string"
                    "Fish\nCow\nMonkey"
                    (getText $ textEdit "Fish\nCow\nMonkey")
            )

          , testProperty "Any string" $ \s -> getText (textEdit s) == s
        ]

      , testGroup "Cursor Positioning" [
            testCase "Basic string" (
                assertEqual
                    "Should be at end of string"
                    (4, 0)
                    (getCursor $ textEdit "Blah")
            )

          , testCase "New lines" (
                assertEqual
                    "Should be at end of string"
                    (6, 2)
                    (getCursor $ textEdit "Fish\nCow\nMonkey")
            )

          , testCase "New line at end" (
                assertEqual
                    "Should be at end of string"
                    (0, 3)
                    (getCursor $ textEdit "Fish\nCow\nMonkey\n")
            )
        ]

      , testGroup "Wrapped Cursor Positioning" [
            testGroup "No wrapping necessary" [
                testCase "Basic string" (
                    assertEqual
                        "Should be at end of string"
                        (["Blah"], (4, 0))
                        (getWrapped 30 $ textEdit "Blah")
                )

              , testCase "New lines" (
                    assertEqual
                        "Should be at end of string"
                        (["Fish", "Cow", "Monkey"], (6, 2))
                        (getWrapped 30 $ textEdit "Fish\nCow\nMonkey")
                )

              , testCase "New line at end" (
                    assertEqual
                        "Should be at end of string"
                        (["Fish", "Cow", "Monkey", ""], (0, 3))
                        (getWrapped 30 $ textEdit "Fish\nCow\nMonkey\n")
                )
            ]

          , testGroup "Wrapping necessary" [
                testCase "Basic string" (
                    assertEqual
                        "Should be at end of string"
                        (["Blah blah ", "blah"], (4, 1))
                        (getWrapped 10 $ textEdit "Blah blah blah")
                )
            ]
        ]
    ]
