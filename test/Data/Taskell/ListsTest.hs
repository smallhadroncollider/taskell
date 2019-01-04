{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.ListsTest
    ( test_lists
    ) where

import ClassyPrelude hiding (delete)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Taskell.List           as L
import           Data.Taskell.Lists.Internal
import qualified Data.Taskell.Task           as T

-- test data
list1, list2, list3 :: L.List
list1 = foldl' (flip L.append) (L.empty "List 1") [T.new "One", T.new "Two", T.new "Three"]

list2 = foldl' (flip L.append) (L.empty "List 2") [T.new "1", T.new "2", T.new "3"]

list3 = foldl' (flip L.append) (L.empty "List 3") [T.new "01", T.new "10", T.new "11"]

testLists :: Lists
testLists = fromList [list1, list2, list3]

-- tests
test_lists :: TestTree
test_lists =
    testGroup
        "Data.Taskell.Lists"
        [ testCase
              "initial"
              (assertEqual
                   "Returns To Do and Done lists"
                   (fromList [L.empty "To Do", L.empty "Done"])
                   initial)
        , testCase
              "updateLists"
              (assertEqual
                   "Replaces the middle list"
                   (fromList [list1, list1, list3])
                   (updateLists 1 list1 testLists))
        , testGroup
              "count"
              [ testCase
                    "list exists"
                    (assertEqual "Returns length of middle list" 3 (count 1 testLists))
              , testCase "list does not exist" (assertEqual "Returns 0" 0 (count 10 testLists))
              ]
        , testGroup
              "get"
              [ testCase
                    "list exists"
                    (assertEqual "Returns the list" (Just list2) (get testLists 1))
              , testCase "list does not exist" (assertEqual "Nothing" Nothing (get testLists 10))
              ]
        , testGroup
              "changeList"
              [ testCase
                    "right"
                    (assertEqual
                         "Returns updated lists"
                         (Just
                              (fromList
                                   [ list1
                                   , foldl'
                                         (flip L.append)
                                         (L.empty "List 2")
                                         [T.new "1", T.new "3"]
                                   , foldl'
                                         (flip L.append)
                                         (L.empty "List 3")
                                         [T.new "01", T.new "10", T.new "11", T.new "2"]
                                   ]))
                         (changeList (1, 1) testLists 1))
              , testCase
                    "left"
                    (assertEqual
                         "Returns updated lists"
                         (Just
                              (fromList
                                   [ foldl'
                                         (flip L.append)
                                         (L.empty "List 1")
                                         [T.new "One", T.new "Two", T.new "Three", T.new "2"]
                                   , foldl'
                                         (flip L.append)
                                         (L.empty "List 2")
                                         [T.new "1", T.new "3"]
                                   , list3
                                   ]))
                         (changeList (1, 1) testLists (-1)))
              , testCase
                    "out of bounds list"
                    (assertEqual "Nothing" Nothing (changeList (5, 1) testLists 1))
              , testCase
                    "out of bounds task"
                    (assertEqual "Nothing" Nothing (changeList (1, 10) testLists 1))
              ]
        , testCase
              "newList"
              (assertEqual
                   "Returns lists with new list"
                   (fromList [list1, list2, list3, L.empty "Hello"])
                   (newList "Hello" testLists))
        , testCase
              "delete"
              (assertEqual
                   "Returns lists with middle list removed"
                   (fromList [list1, list3])
                   (delete 1 testLists))
        , testGroup
              "exists"
              [ testCase "list exists" (assertEqual "Returns True" True (exists 1 testLists))
              , testCase
                    "list does not exist"
                    (assertEqual "Returns False" False (exists 10 testLists))
              ]
        , testGroup
              "shiftBy"
              [ testCase
                    "right"
                    (assertEqual
                         "Returns updated lists"
                         (Just (fromList [list1, list3, list2]))
                         (shiftBy 1 1 testLists))
              , testCase
                    "left"
                    (assertEqual
                         "Returns updated lists"
                         (Just (fromList [list2, list1, list3]))
                         (shiftBy 1 (-1) testLists))
              , testCase
                    "out of bounds list"
                    (assertEqual "Nothing" Nothing (shiftBy 5 1 testLists))
              , testCase
                    "out of bounds shift"
                    (assertEqual
                         "Returns updated lists"
                         (Just (fromList [list2, list1, list3]))
                         (shiftBy 1 (-10) testLists))
              ]
        , testGroup
              "search"
              [ testCase
                    "term exists"
                    (assertEqual
                         "Returns filtered lists"
                         (fromList
                              [ foldl' (flip L.append) (L.empty "List 1") [T.new "One"]
                              , L.empty "List 2"
                              , L.empty "List 3"
                              ])
                         (search "One" testLists))
              , testCase
                    "term doesn't exist"
                    (assertEqual
                         "Returns updated lists"
                         (fromList [L.empty "List 1", L.empty "List 2", L.empty "List 3"])
                         (search "Fish" testLists))
              ]
        , testGroup
              "appendToLast"
              [ testCase
                    "previous list exists"
                    (assertEqual
                         "Returns updated lists"
                         (fromList
                              [ list1
                              , list2
                              , foldl'
                                    (flip L.append)
                                    (L.empty "List 3")
                                    [T.new "01", T.new "10", T.new "11", T.new "Blah"]
                              ])
                         (appendToLast (T.new "Blah") testLists))
              , testCase
                    "previous list doesn't exist"
                    (assertEqual "Returns original list" empty (appendToLast (T.new "Blah") empty))
              ]
        , testCase
              "analyse"
              (assertEqual
                   "Returns an analysis"
                   "test.md\nLists: 3\nTasks: 9"
                   (analyse "test.md" testLists))
        ]
