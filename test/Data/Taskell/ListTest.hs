{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.ListTest (
    test_list
) where

import ClassyPrelude as CP

import Control.Lens ((.~))

import Test.Tasty
import Test.Tasty.HUnit

import Data.Taskell.List.Internal as L
import qualified Data.Taskell.Task as T (Task, blank, new, name)

emptyList :: List
emptyList = L.empty "Test"

taskSeq :: Seq T.Task
taskSeq = fromList [T.new "Hello", T.new "Blah", T.new "Fish"]

populatedList :: List
populatedList = List "Populated" taskSeq

-- tests
test_list :: TestTree
test_list =
    testGroup "Data.Taskell.List" [
        testCase "create" (
            assertEqual
                "Empty list with title"
                (List "Test" CP.empty)
                (create "Test" CP.empty)
        )

      , testCase "empty" (
            assertEqual
                "Empty list with title"
                (List "Test" CP.empty)
                emptyList
        )

      , testCase "new" (
            assertEqual
                "List with title and blank Task"
                (List "Test" (fromList [T.blank]))
                (new emptyList)
        )

      , testGroup "count" [
            testCase "empty" (
                assertEqual
                    "List length"
                    0
                    (count emptyList)
            )

          , testCase "one item" (
                assertEqual
                    "List length"
                    1
                    (count $ new emptyList)
            )
        ]

      , testCase "newAt" (
            assertEqual
                "List with new item second position"
                (List "Populated" (fromList [T.new "Hello", T.blank, T.new "Blah", T.new "Fish"]))
                (newAt 1 populatedList)
        )

      , testGroup "append" [
            testCase "populated" (
                assertEqual
                    "List with new item"
                    (List "Populated" (fromList [T.new "Hello", T.new "Blah", T.new "Fish", T.new "Spoon"]))
                    (append (T.new "Spoon") populatedList)
            )

          , testCase "empty" (
                assertEqual
                    "List with new item"
                    (List "Test" (fromList [T.new "Spoon"]))
                    (append (T.new "Spoon") emptyList)
            )
        ]

      , testCase "extract" (
            assertEqual
                "List and extracted item"
                (Just (List "Populated" (fromList [T.new "Hello", T.new "Fish"]), T.new "Blah"))
                (extract 1 populatedList)
        )

      , testCase "updateFn" (
            assertEqual
                "List with updated item"
                (List "Populated" (fromList [T.new "Hello", T.new "Monkey", T.new "Fish"]))
                (updateFn 1 (T.name .~ "Monkey") populatedList)
        )

      , testCase "update" (
            assertEqual
                "List with updated item"
                (List "Populated" (fromList [T.new "Hello", T.new "Monkey", T.new "Fish"]))
                (update 1 (T.new "Monkey") populatedList)
        )

      , testGroup "move" [
            testCase "up" (
                assertEqual
                    "List with moved item"
                    (Just (List "Populated" (fromList [T.new "Hello", T.new "Fish", T.new "Blah"])))
                    (move 1 1 populatedList)
            )

          , testCase "down" (
                assertEqual
                    "List with moved item"
                    (Just (List "Populated" (fromList [T.new "Blah", T.new "Hello", T.new "Fish"])))
                    (move 1 (-1) populatedList)
            )

          , testCase "up - out of bounds" (
                assertEqual
                    "List with moved item"
                    (Just (List "Populated" (fromList [T.new "Hello", T.new "Fish", T.new "Blah"])))
                    (move 1 10 populatedList)
            )

          , testCase "down - out of bounds" (
                assertEqual
                    "List with moved item"
                    (Just (List "Populated" (fromList [T.new "Blah", T.new "Hello", T.new "Fish"])))
                    (move 1 (-10) populatedList)
            )
        ]

      , testCase "deleteTask" (
            assertEqual
                "List with removed item"
                (List "Populated" (fromList [T.new "Hello", T.new "Fish"]))
                (deleteTask 1 populatedList)
        )

      , testGroup "getTask" [
            testCase "exists" (
                assertEqual
                    "Middle item"
                    (Just (T.new "Blah"))
                    (getTask 1 populatedList)
            )

          , testCase "doesn't exist" (
                assertEqual
                    "Nothing"
                    Nothing
                    (getTask 5 populatedList)
            )
        ]

      , testGroup "searchFor" [
            testCase "exists" (
                assertEqual
                    "Blah item"
                    (List "Populated" (fromList [T.new "Blah"]))
                    (searchFor "Bl" populatedList)
            )

          , testCase "multiple" (
                assertEqual
                    "Blah and Fish"
                    (List "Populated" (fromList [T.new "Blah", T.new "Fish"]))
                    (searchFor "h" populatedList)
            )

          , testCase "doesn't exist" (
                assertEqual
                    "Empty list"
                    (List "Populated" CP.empty)
                    (searchFor "FFF" populatedList)
            )
        ]
    ]
