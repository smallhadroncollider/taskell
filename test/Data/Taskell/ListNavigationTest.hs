{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.ListNavigationTest
    ( test_list
    ) where

import ClassyPrelude as CP

import Test.Tasty
import Test.Tasty.HUnit

import           Data.Taskell.List.Internal as L
import qualified Data.Taskell.Task          as T (Task, new)

taskSeq :: Seq T.Task
taskSeq =
    fromList
        [ T.new "Hello"
        , T.new "Blah"
        , T.new "Fish"
        , T.new "Spoons"
        , T.new "Hello Again!"
        , T.new "Computers"
        ]

list :: List
list = List "Populated" taskSeq

-- tests
test_list :: TestTree
test_list =
    testGroup
        "Data.Taskell.List Navigation"
        [ testGroup
              "next"
              [ testCase "no term" (assertEqual "1" 1 (L.nextTask 0 Nothing list))
              , testCase "with term" (assertEqual "4" 4 (L.nextTask 0 (Just "Hello") list))
              , testCase
                    "with term - no match"
                    (assertEqual "0" 0 (L.nextTask 0 (Just "Wombat") list))
              ]
        , testGroup
              "prev"
              [ testCase "no term" (assertEqual "0" 0 (L.prevTask 1 Nothing list))
              , testCase "with term" (assertEqual "0" 0 (L.prevTask 4 (Just "Hello") list))
              , testCase
                    "with term - no match"
                    (assertEqual "4" 4 (L.prevTask 4 (Just "Wombat") list))
              ]
        , testGroup
              "nearest"
              [ testCase "no term" (assertEqual "same task" 2 (L.nearest 2 Nothing list))
              , testCase "term matches" (assertEqual "same task" 2 (L.nearest 2 (Just "ish") list))
              , testCase "term after" (assertEqual "next task" 3 (L.nearest 2 (Just "oons") list))
              , testCase
                    "term before"
                    (assertEqual "previous task" 3 (L.nearest 5 (Just "oons") list))
              , testCase
                    "term both - before closer"
                    (assertEqual "previous task" 0 (L.nearest 2 (Just "Hello") list))
              , testCase
                    "term both - after closer"
                    (assertEqual "next task" 4 (L.nearest 3 (Just "Hello") list))
              , testCase
                    "no matches"
                    (assertEqual "nothing" (-1) (L.nearest 3 (Just "Penguins") list))
              , testCase
                    "nothing to match"
                    (assertEqual "nothing" 3 (L.nearest (-1) (Just "Spoon") list))
              , testCase
                    "nothing with no term"
                    (assertEqual "nothing" 0 (L.nearest (-1) Nothing list))
              , testCase
                    "out of bounds with no term"
                    (assertEqual "nothing" 5 (L.nearest 50 Nothing list))
              ]
        ]
