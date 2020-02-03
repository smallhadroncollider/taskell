{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Taskell.IO.Keyboard.ParserTest
    ( test_parser
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.FileEmbed    (embedFile)
import Text.RawString.QQ (r)

import qualified Taskell.Events.Actions.Types as A
import           Taskell.IO.Keyboard.Parser   (bindings)
import           Taskell.IO.Keyboard.Types

basic :: Text
basic = "quit = q"

basicResult :: Bindings
basicResult = [(BChar 'q', A.Quit)]

basicMulti :: Text
basicMulti =
    [r|
        quit = q
        detail = <Enter>
    |]

basicMultiResult :: Bindings
basicMultiResult = [(BChar 'q', A.Quit), (BKey "Enter", A.Detail)]

ini :: Text
ini = decodeUtf8 $(embedFile "test/Taskell/IO/Keyboard/data/bindings.ini")

iniResult :: Bindings
iniResult =
    [ (BChar 'q', A.Quit)
    , (BChar 'u', A.Undo)
    , (BChar 'r', A.Redo)
    , (BChar '/', A.Search)
    , (BChar '?', A.Help)
    , (BChar '!', A.Due)
    , (BChar 'k', A.Previous)
    , (BChar 'j', A.Next)
    , (BChar 'h', A.Left)
    , (BChar 'l', A.Right)
    , (BChar 'G', A.Bottom)
    , (BChar 'a', A.New)
    , (BChar 'O', A.NewAbove)
    , (BChar 'o', A.NewBelow)
    , (BChar '+', A.Duplicate)
    , (BChar 'e', A.Edit)
    , (BChar 'A', A.Edit)
    , (BChar 'i', A.Edit)
    , (BChar 'C', A.Clear)
    , (BChar 'D', A.Delete)
    , (BKey "Enter", A.Detail)
    , (BChar '@', A.DueDate)
    , (BKey "Backspace", A.ClearDate)
    , (BChar 'K', A.MoveUp)
    , (BChar 'J', A.MoveDown)
    , (BChar 'H', A.MoveLeft)
    , (BChar 'L', A.MoveRight)
    , (BKey "Space", A.Complete)
    , (BChar 'm', A.MoveMenu)
    , (BChar 'N', A.ListNew)
    , (BChar 'E', A.ListEdit)
    , (BChar 'X', A.ListDelete)
    , (BChar '>', A.ListRight)
    , (BChar '<', A.ListLeft)
    ]

comma :: Text
comma = "quit = ,"

commaResult :: Bindings
commaResult = [(BChar ',', A.Quit)]

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
