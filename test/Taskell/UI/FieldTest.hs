module Taskell.UI.FieldTest
    ( test_field
    ) where

import ClassyPrelude

import Taskell.UI.Draw.Field (Field (Field), backspace, cursorPosition, insertCharacter, insertText,
                              updateCursor, wrap)

import Test.Tasty
import Test.Tasty.HUnit

width :: Int
width = 30

cursorPosition' :: Text -> Int -> (Int, Int)
cursorPosition' text cursor = cursorPosition wrapped width (cursor - offset)
  where
    (wrapped, offset) = wrap width text

test_field :: TestTree
test_field =
    testGroup
        "UI.Draw.Field"
        [ testGroup
              "Field cursor left"
              [ testCase
                    "Standard"
                    (assertEqual
                         "Should move left"
                         (Field "Blah" 1)
                         (updateCursor (-1) (Field "Blah" 2)))
              , testCase
                    "Out of bounds"
                    (assertEqual
                         "Should stay 0"
                         (Field "Blah" 0)
                         (updateCursor (-1) (Field "Blah" 0)))
              ]
        , testGroup
              "Field cursor right"
              [ testCase
                    "Standard"
                    (assertEqual
                         "Should move right"
                         (Field "Blah" 3)
                         (updateCursor 1 (Field "Blah" 2)))
              , testCase
                    "At end"
                    (assertEqual
                         "Should move right"
                         (Field "Blah" 4)
                         (updateCursor 1 (Field "Blah" 3)))
              , testCase
                    "Out of bounds"
                    (assertEqual "Should stay" (Field "Blah" 4) (updateCursor 1 (Field "Blah" 4)))
              ]
        , testGroup
              "Field insert character"
              [ testCase
                    "At end"
                    (assertEqual
                         "Should insert character at end"
                         (Field "Blahs" 5)
                         (insertCharacter 's' (Field "Blah" 4)))
              , testCase
                    "At beginning"
                    (assertEqual
                         "Should insert character at beginning"
                         (Field "sBlah" 1)
                         (insertCharacter 's' (Field "Blah" 0)))
              ]
        , testGroup
              "Field backspace"
              [ testCase
                    "At end"
                    (assertEqual
                         "Should remove last character"
                         (Field "Bla" 3)
                         (backspace (Field "Blah" 4)))
              , testCase
                    "At beginning"
                    (assertEqual
                         "Should stay the same"
                         (Field "Blah" 0)
                         (backspace (Field "Blah" 0)))
              , testCase
                    "In middle"
                    (assertEqual "Should remove middle" (Field "Bah" 1) (backspace (Field "Blah" 2)))
              ]
        , testGroup
              "Field insert text"
              [ testCase
                    "At end"
                    (assertEqual
                         "Should insert text at end"
                         (Field "Blah hello" 10)
                         (insertText " hello" (Field "Blah" 4)))
              , testCase
                    "At beginning"
                    (assertEqual
                         "Should insert text at beginning"
                         (Field "Hello Blah" 6)
                         (insertText "Hello " (Field "Blah" 0)))
              ]
        , testGroup
              "Cursor position"
              [ testCase
                    "Empty"
                    (assertEqual "Should be at beginning" (0, 0) (cursorPosition' "" 0))
              , testCase
                    "First line"
                    (assertEqual
                         "Should be on first line"
                         (14, 0)
                         (cursorPosition' "Blah blah blah" 14))
              , testCase
                    "Half way along"
                    (assertEqual
                         "Should be on first line"
                         (7, 0)
                         (cursorPosition' "Blah blah blah" 7))
              , testCase
                    "End of line"
                    (assertEqual
                         "Should be on first line"
                         (29, 0)
                         (cursorPosition' "Blah blah blah blah blah blah" 29))
              , testCase
                    "End of line wrap"
                    (assertEqual
                         "Should wrap"
                         (0, 1)
                         (cursorPosition' "Blah blah blah blah blah blahs" 30))
              , testCase
                    "End of line with space wrap"
                    (assertEqual
                         "Should wrap"
                         (0, 1)
                         (cursorPosition' "Blah blah blah blah blah blah " 30))
              , testCase
                    "Long words"
                    (assertEqual
                         "Should wrap to correct position"
                         (6, 1)
                         (cursorPosition' "Artichoke penguin astronaut wombat" 34))
              , testCase
                    "Triple line"
                    (assertEqual
                         "Should wrap to correct position"
                         (11, 2)
                         (cursorPosition'
                              "Artichoke penguin astronaut wombat artichoke penguin astronaut wombat"
                              64))
              , testCase
                    "Long words with space"
                    (assertEqual
                         "Should ignore the space when counting"
                         (16, 1)
                         (cursorPosition' "Blah fish wombat monkey sponge catpult arsonist" 47))
              , testCase
                    "Multibyte string single line"
                    (assertEqual "Should move in twos" (4, 0) (cursorPosition' "乤乭亍乫" 2))
              , testCase
                    "Multibyte string multi-line"
                    (assertEqual
                         "Should move in twos"
                         (4, 1)
                         (cursorPosition' "乤乭亍乫 乤乭亍乫 乤乭亍乫 乤乭亍乫" 17))
              ]
        ]
