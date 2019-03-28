{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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

import IO.Keyboard.Parser (bindings)
import IO.Keyboard.Types

basic :: Text
basic = "quit = q"

basicResult :: Bindings
basicResult = [(BChar 'q', "quit")]

basicMulti :: Text
basicMulti =
    [r|
        quit = q
        detail = <Enter>
    |]

basicMultiResult :: Bindings
basicMultiResult = [(BChar 'q', "quit"), (BKey "Enter", "detail")]

ini :: Text
ini = decodeUtf8 $(embedFile "test/IO/Keyboard/data/bindings.ini")

iniResult :: Bindings
iniResult =
    [ (BChar 'q', "quit")
    , (BChar 'u', "undo")
    , (BChar '/', "search")
    , (BChar '?', "help")
    , (BChar 'k', "previous")
    , (BChar 'j', "next")
    , (BChar 'h', "left")
    , (BChar 'l', "right")
    , (BChar 'g', "bottom")
    , (BChar 'a', "new")
    , (BChar 'O', "newAbove")
    , (BChar 'o', "newBelow")
    , (BChar 'e', "edit")
    , (BChar 'A', "edit")
    , (BChar 'i', "edit")
    , (BChar 'C', "clear")
    , (BChar 'D', "delete")
    , (BKey "Enter", "detail")
    , (BChar '@', "dueDate")
    , (BChar 'K', "moveUp")
    , (BChar 'J', "moveDown")
    , (BChar 'H', "moveLeft")
    , (BChar 'L', "moveRight")
    , (BKey "Space", "moveRight")
    , (BChar 'm', "moveMenu")
    , (BChar 'N', "listNew")
    , (BChar 'E', "listEdit")
    , (BChar 'X', "listDelete")
    , (BChar '>', "listRight")
    , (BChar '<', "listLeft")
    ]

test_parser :: TestTree
test_parser =
    testGroup
        "IO.Keyboard.Parser"
        [ testCase "basic" (assertEqual "Parses quit" (Right basicResult) (bindings basic))
        , testCase
              "basic multiline"
              (assertEqual "Parses both" (Right basicMultiResult) (bindings basicMulti))
        , testCase "full ini file" (assertEqual "Parses all" (Right iniResult) (bindings ini))
        ]
