{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.Data.SeqTest
    ( test_seq
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Taskell.Data.Seq

testSeq :: Seq Int
testSeq = fromList [1, 2, 3, 4, 5]

-- tests
test_seq :: TestTree
test_seq =
    testGroup
        "Data.Taskell.Seq"
        [ testGroup
              "extract"
              [ testCase
                    "Standard"
                    (assertEqual
                         "Extracts third item"
                         (Just (fromList [1, 2, 4, 5], 3))
                         (extract 2 testSeq))
              , testCase
                    "First"
                    (assertEqual
                         "Extracts first item"
                         (Just (fromList [2, 3, 4, 5], 1))
                         (extract 0 testSeq))
              , testCase
                    "Last"
                    (assertEqual
                         "Extracts last item"
                         (Just (fromList [1, 2, 3, 4], 5))
                         (extract 4 testSeq))
              , testCase
                    "Out of range - positive"
                    (assertEqual "Nothing" Nothing (extract 5 testSeq))
              , testCase
                    "Out of range - negative"
                    (assertEqual "Nothing" Nothing (extract (-1) testSeq))
              ]
        , testGroup
              "shiftBy"
              [ testCase
                    "To right"
                    (assertEqual
                         "Moves third item right"
                         (Just (fromList [1, 2, 4, 3, 5]))
                         (shiftBy 2 1 testSeq))
              , testCase
                    "To left"
                    (assertEqual
                         "Moves third item left"
                         (Just (fromList [1, 3, 2, 4, 5]))
                         (shiftBy 2 (-1) testSeq))
              , testCase
                    "First to right"
                    (assertEqual
                         "Moves first item right"
                         (Just (fromList [2, 1, 3, 4, 5]))
                         (shiftBy 0 1 testSeq))
              , testCase
                    "First to left"
                    (assertEqual "The original sequence" (Just testSeq) (shiftBy 0 (-1) testSeq))
              , testCase
                    "Last to right"
                    (assertEqual "The original sequence" (Just testSeq) (shiftBy 4 1 testSeq))
              , testCase
                    "Last to left"
                    (assertEqual
                         "Moves last item left"
                         (Just (fromList [1, 2, 3, 5, 4]))
                         (shiftBy 4 (-1) testSeq))
              , testCase
                    "Ridiculous right shift"
                    (assertEqual
                         "Moves to right-most"
                         (Just (fromList [1, 2, 4, 5, 3]))
                         (shiftBy 2 20 testSeq))
              , testCase
                    "Ridiculous left shift"
                    (assertEqual
                         "Moves to left-most"
                         (Just (fromList [3, 1, 2, 4, 5]))
                         (shiftBy 2 (-20) testSeq))
              ]
        ]
