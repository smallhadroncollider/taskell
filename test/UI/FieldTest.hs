{-# LANGUAGE OverloadedStrings #-}
module UI.FieldTest where

import UI.Field (Field(Field), updateCursor, insertCharacter, insertText, cursorPosition)

import Test.Tasty
import Test.Tasty.HUnit

test_field :: TestTree
test_field =
        testGroup "UI.Field" [
            testGroup "Field cursor left" [
                testCase "Standard"
                    (assertEqual "Should move left" (Field "Blah" 1) (updateCursor (-1) (Field "Blah" 2)))
              , testCase "Out of bounds"
                    (assertEqual "Should stay 0" (Field "Blah" 0) (updateCursor (-1) (Field "Blah" 0)))
            ]

          , testGroup "Field cursor right" [
                testCase "Standard"
                    (assertEqual "Should move right" (Field "Blah" 3) (updateCursor 1 (Field "Blah" 2)))
              , testCase "At end"
                    (assertEqual "Should move right" (Field "Blah" 4) (updateCursor 1 (Field "Blah" 3)))
              , testCase "Out of bounds"
                    (assertEqual "Should stay" (Field "Blah" 4) (updateCursor 1 (Field "Blah" 4)))
            ]

          , testGroup "Field insert character" [
                testCase "At end"
                    (assertEqual "Should insert character at end" (Field "Blahs" 5) (insertCharacter 's' (Field "Blah" 4)))
              , testCase "At beginning"
                    (assertEqual "Should insert character at beginning" (Field "sBlah" 1) (insertCharacter 's' (Field "Blah" 0)))
            ]

          , testGroup "Field insert text" [
                testCase "At end"
                    (assertEqual "Should insert text at end" (Field "Blah hello" 10) (insertText " hello" (Field "Blah" 4)))
              , testCase "At beginning"
                    (assertEqual "Should insert text at beginning" (Field "Hello Blah" 6) (insertText "Hello " (Field "Blah" 0)))
            ]

          , testGroup "Cursor position" [
                testCase "Empty"
                    (assertEqual "Should (0,0)" (0, 0) (cursorPosition [] 0))
              , testCase "One line"
                    (assertEqual "Should return on same line" (3, 0) (cursorPosition ["Blah"] 3))
              , testCase "Two line"
                    (assertEqual "Should return on second line" (1, 1) (cursorPosition ["Blah ", "Blaz"] 6))
              , testCase "Early cursor"
                    (assertEqual "Should return on first line" (2, 0) (cursorPosition ["Blah ", "Blaz"] 2))
              , testCase "End of line"
                    (assertEqual "Should return on next line" (0, 1) (cursorPosition ["Blah ", "Blaz"] 5))
            ]
        ]
