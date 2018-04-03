{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.MarkdownTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import IO.Markdown.Internal (start)
import IO.Config (defaultMarkdownConfig)
import Data.Taskell.Lists (newList)

test_markdown :: TestTree
test_markdown =
    testGroup "IO.Markdown" [
        testGroup "Parsing" [
            testCase "List Title" (
                assertEqual
                    "One list"
                    (newList "Test" empty, [])
                    (start defaultMarkdownConfig (empty, []) ("## Test", 1))
            )
        ]
    ]
