{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module IO.Keyboard.ParserTest
    ( test_parser
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.FileEmbed    (embedFile)
import Text.RawString.QQ (r)

import Events.Actions.Types
import IO.Keyboard.Parser   (bindings)
import IO.Keyboard.Types

basic :: Text
basic = "quit = q"

basicResult :: Bindings
basicResult = [(BChar 'q', AQuit)]

basicMulti :: Text
basicMulti =
    [r|
        quit = q
        detail = <Enter>
    |]

basicMultiResult :: Bindings
basicMultiResult = [(BChar 'q', AQuit), (BKey "Enter", ADetail)]

ini :: Text
ini = decodeUtf8 $(embedFile "test/IO/Keyboard/data/bindings.ini")

iniResult :: Bindings
iniResult =
    [ (BChar 'q', AQuit)
    , (BChar 'u', AUndo)
    , (BChar '/', ASearch)
    , (BChar '?', AHelp)
    , (BChar 'k', APrevious)
    , (BChar 'j', ANext)
    , (BChar 'h', ALeft)
    , (BChar 'l', ARight)
    , (BChar 'g', ABottom)
    , (BChar 'a', ANew)
    , (BChar 'O', ANewAbove)
    , (BChar 'o', ANewBelow)
    , (BChar 'e', AEdit)
    , (BChar 'A', AEdit)
    , (BChar 'i', AEdit)
    , (BChar 'C', AClear)
    , (BChar 'D', ADelete)
    , (BKey "Enter", ADetail)
    , (BChar '@', ADueDate)
    , (BChar 'K', AMoveUp)
    , (BChar 'J', AMoveDown)
    , (BChar 'H', AMoveLeft)
    , (BChar 'L', AMoveRight)
    , (BKey "Space", AMoveRight)
    , (BChar 'm', AMoveMenu)
    , (BChar 'N', AListNew)
    , (BChar 'E', AListEdit)
    , (BChar 'X', AListDelete)
    , (BChar '>', AListRight)
    , (BChar '<', AListLeft)
    ]

comma :: Text
comma = "quit = ,"

commaResult :: Bindings
commaResult = [(BChar ',', AQuit)]

test_parser :: TestTree
test_parser =
    testGroup
        "IO.Keyboard.Parser"
        [ testCase "basic" (assertEqual "Parses quit" (Right basicResult) (bindings basic))
        , testCase
              "basic multiline"
              (assertEqual "Parses both" (Right basicMultiResult) (bindings basicMulti))
        , testCase "full ini file" (assertEqual "Parses all" (Right iniResult) (bindings ini))
        , testCase "comma" (assertEqual "Parses comma" (Right commaResult) (bindings comma))
        ]
