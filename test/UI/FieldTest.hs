{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.FieldTest (
    test_field
) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (ignoreTest)

import UI.Field (Field(Field), updateCursor, insertCharacter, insertText, cursorPosition, backspace)

width :: Int
width = 30

test_field :: TestTree
test_field =
    testGroup "UI.Field" [
        testGroup "Field cursor left" [
            testCase "Standard" (
                assertEqual
                    "Should move left"
                    (Field "Blah" 1)
                    (updateCursor (-1) (Field "Blah" 2))
            )

          , testCase "Out of bounds" (
                assertEqual
                    "Should stay 0"
                    (Field "Blah" 0)
                    (updateCursor (-1) (Field "Blah" 0))
            )
        ]

      , testGroup "Field cursor right" [
            testCase "Standard" (
                assertEqual
                    "Should move right"
                    (Field "Blah" 3)
                    (updateCursor 1 (Field "Blah" 2))
            )

          , testCase "At end" (
                assertEqual
                    "Should move right"
                    (Field "Blah" 4)
                    (updateCursor 1 (Field "Blah" 3))
            )

          , testCase "Double space" (
                assertEqual
                    "Should move right"
                    (Field "Blah  " 5)
                    (updateCursor 1 (Field "Blah  " 4))
            )

          , testCase "Out of bounds" (
                assertEqual
                    "Should stay"
                    (Field "Blah" 4)
                    (updateCursor 1 (Field "Blah" 4))
            )
        ]

      , testGroup "Field insert character" [
            testCase "At end" (
                assertEqual
                    "Should insert character at end"
                    (Field "Blahs" 5)
                    (insertCharacter 's' (Field "Blah" 4))
            )

          , testCase "At beginning" (
                assertEqual
                    "Should insert character at beginning"
                    (Field "sBlah" 1)
                    (insertCharacter 's' (Field "Blah" 0))
            )

          , testCase "After a space" (
                assertEqual
                    "Should insert character after the space"
                    (Field "Blah sblah" 6)
                    (insertCharacter 's' (Field "Blah blah" 5))
            )

          , testCase "In a space" (
                assertEqual
                    "Should insert character before the space"
                    (Field "Blahs blah" 5)
                    (insertCharacter 's' (Field "Blah blah" 4))
            )
        ]

      , testGroup "Field backspace" [
            testCase "At end" (
                assertEqual
                    "Should remove last character"
                    (Field "Bla" 3)
                    (backspace (Field "Blah" 4))
            )

          , testCase "At beginning" (
                assertEqual
                    "Should stay the same"
                    (Field "Blah" 0)
                    (backspace (Field "Blah" 0))
            )

          , testCase "In middle" (
                assertEqual
                    "Should remove middle"
                    (Field "Bah" 1)
                    (backspace (Field "Blah" 2))
            )
        ]

      , testGroup "Field insert text" [
            testCase "At end" (
                assertEqual
                    "Should insert text at end"
                    (Field "Blah hello" 10)
                    (insertText " hello" (Field "Blah" 4))
            )

          , testCase "At beginning" (
                assertEqual
                    "Should insert text at beginning"
                    (Field "Hello Blah" 6)
                    (insertText "Hello " (Field "Blah" 0))
            )
        ]

      , testGroup "Cursor position" [
            testCase "Empty" (
                assertEqual
                    "Should be at beginning"
                    (0, 0)
                    (cursorPosition [("", 0)] width 0)
            )

          , testCase "First line" (
                assertEqual
                    "Should be on first line"
                    (14, 0)
                    (cursorPosition [("Blah blah blah", 0)] width 14)
            )

          , testCase "Half way along" (
                assertEqual
                    "Should be on first line"
                    (7, 0)
                    (cursorPosition [("Blah blah blah", 0)] width 7)
            )

          , testCase "End of line" (
                assertEqual
                    "Should be on first line"
                    (29, 0)
                    (cursorPosition [("Blah blah blah blah blah blah", 0)] width 29)
            )

          , testCase "End of line wrap" (
                assertEqual
                    "Should wrap"
                    (0, 1)
                    (cursorPosition [("Blah blah blah blah blah blahs", 0)] width 30)
            )

          , testCase "End of line with space wrap" (
                assertEqual
                    "Should wrap"
                    (0, 1)
                    (cursorPosition [("Blah blah blah blah blah blah ", 0)] width 30)
            )

          , testCase "Long words" (
                assertEqual
                    "Should wrap to correct position"
                    (6, 1)
                    (cursorPosition [
                        ("Artichoke penguin astronaut ", 0)
                      , ("wombat", 0)
                    ] width 34)
            )

          , ignoreTest $ testCase "Long words with space" (
                assertEqual
                    "Should ignore the space when counting"
                    (16, 1)
                    (cursorPosition [
                        ("Blah fish wombat monkey sponge", 0)
                      , ("catpult arsonist", 1)
                    ] width 47)
            )
        ]

      , testGroup "New lines" [
            testCase "Before new line" (
                assertEqual
                    "Should return first line"
                    (2, 0)
                    (cursorPosition [("Blah", 1), ("Blah", 0)] width 2)
            )

          , testCase "Immediately before new line" (
                assertEqual
                    "Should return first line"
                    (3, 0)
                    (cursorPosition [("Blah", 1), ("Blah", 0)] width 3)
            )

          , testCase "On new line" (
                assertEqual
                    "Should return second line"
                    (4, 0)
                    (cursorPosition [("Blah", 1), ("Blah", 0)] width 4)
            )

          , ignoreTest $ testCase "Start of new line" (
                assertEqual
                    "Should return on second line"
                    (0, 1)
                    (cursorPosition [("Blah", 1), ("Blah", 0)] width 5)
            )

          , testCase "After new line" (
                assertEqual
                    "Should return on second line"
                    (2, 1)
                    (cursorPosition [("Blah", 1), ("Blah", 0)] width 7)
            )

        ]
    ]
