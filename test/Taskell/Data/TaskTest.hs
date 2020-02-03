{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.Data.TaskTest
    ( test_task
    ) where

import ClassyPrelude

import Control.Lens ((.~))

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time.Calendar (fromGregorianValid)

import           Taskell.Data.Date          (Due (DueDate))
import qualified Taskell.Data.Subtask       as ST (name, new)
import           Taskell.Data.Task.Internal

desc :: Maybe Text
desc = Just "A very boring description"

testTask :: Task
testTask =
    Task
    { _name = "Test"
    , _description = desc
    , _subtasks = fromList [ST.new "One" True, ST.new "Two" False, ST.new "Three" False]
    , _due = Nothing
    }

-- tests
test_task :: TestTree
test_task =
    testGroup
        "Data.Taskell.Task"
        [ testCase "blank" (assertEqual "Returns empty task" (Task "" Nothing empty Nothing) blank)
        , testCase
              "new"
              (assertEqual "Returns a new task" (Task "Hello" Nothing empty Nothing) (new "Hello"))
        , testGroup
              "getSubtask"
              [ testCase
                    "exists"
                    (assertEqual
                         "Returns a the sub-task"
                         (Just (ST.new "Two" False))
                         (getSubtask 1 testTask))
              , testCase
                    "doesn't exist"
                    (assertEqual "Returns Nothing" Nothing (getSubtask 10 testTask))
              ]
        , testGroup
              "setDescription"
              [ testCase
                    "empty sub-tasks"
                    (assertEqual
                         "Updates description"
                         (Task "Test" (Just "Description") empty Nothing)
                         (setDescription "Description" (new "Test")))
              , testCase
                    "empty"
                    (assertEqual
                         "Keeps description as Nothing"
                         (new "Test")
                         (setDescription "" (new "Test")))
              , testCase
                    "blank"
                    (assertEqual
                         "Keeps description as Nothing"
                         (new "Test")
                         (setDescription "   " (new "Test")))
              ]
        , testGroup
              "addSubtask"
              [ testCase
                    "existing"
                    (assertEqual
                         "Returns the task with added subtask"
                         (Task
                              "Test"
                              desc
                              (fromList
                                   [ ST.new "One" True
                                   , ST.new "Two" False
                                   , ST.new "Three" False
                                   , ST.new "Four" True
                                   ])
                              Nothing)
                         (addSubtask (ST.new "Four" True) testTask))
              , testCase
                    "empty sub-tasks"
                    (assertEqual
                         "Returns the task with added subtask"
                         (Task "Test" Nothing (fromList [ST.new "One" False]) Nothing)
                         (addSubtask (ST.new "One" False) (new "Test")))
              ]
        , testGroup
              "hasSubtasks"
              [ testCase "existing" (assertEqual "Returns True" True (hasSubtasks testTask))
              , testCase
                    "empty sub-tasks"
                    (assertEqual "Returns False" False (hasSubtasks (new "Test")))
              ]
        , testGroup
              "updateSubtask"
              [ testCase
                    "exists"
                    (assertEqual
                         "Returns updated task"
                         (Task
                              "Test"
                              desc
                              (fromList
                                   [ST.new "One" True, ST.new "Cow" False, ST.new "Three" False])
                              Nothing)
                         (updateSubtask 1 (ST.name .~ "Cow") testTask))
              , testCase
                    "doesn't exist"
                    (assertEqual
                         "Returns task"
                         (Task
                              "Test"
                              desc
                              (fromList
                                   [ST.new "One" True, ST.new "Two" False, ST.new "Three" False])
                              Nothing)
                         (updateSubtask 10 (ST.name .~ "Cow") testTask))
              ]
        , testGroup
              "removeSubtask"
              [ testCase
                    "exists"
                    (assertEqual
                         "Returns updated task"
                         (Task
                              "Test"
                              desc
                              (fromList [ST.new "One" True, ST.new "Three" False])
                              Nothing)
                         (removeSubtask 1 testTask))
              , testCase
                    "doesn't exist"
                    (assertEqual
                         "Returns task"
                         (Task
                              "Test"
                              desc
                              (fromList
                                   [ST.new "One" True, ST.new "Two" False, ST.new "Three" False])
                              Nothing)
                         (removeSubtask 10 testTask))
              ]
        , testCase "countSubtasks" (assertEqual "Returns 3" 3 (countSubtasks testTask))
        , testCase
              "countCompleteSubtasks"
              (assertEqual "Returns 1" 1 (countCompleteSubtasks testTask))
        , testGroup
              "contains"
              [ testCase "in task" (assertEqual "Finds in task" True (contains "Test" testTask))
              , testCase "in sub-task" (assertEqual "Find sub-task" True (contains "One" testTask))
              , testCase
                    "case-insensitive"
                    (assertEqual "Find sub-task" True (contains "ONE" testTask))
              , testCase
                    "case-insensitive"
                    (assertEqual "Find sub-task" True (contains "two" testTask))
              , testCase "missing" (assertEqual "Find sub-task" False (contains "Fish" testTask))
              , testCase
                    "description"
                    (assertEqual "Finds in description" True (contains "boring" testTask))
              , testCase
                    "not in description"
                    (assertEqual "Doesn't find" False (contains "escapades" testTask))
              ]
        , testGroup
              "isBlank"
              [ testCase
                    "blank"
                    (assertEqual "Return True" True (isBlank (Task "" Nothing empty Nothing)))
              , testCase "name not blank" (assertEqual "Returns False" False (isBlank testTask))
              , testCase
                    "description not blank"
                    (assertEqual
                         "Returns False"
                         False
                         (isBlank (Task "" (Just "Blah") empty Nothing)))
              , testCase
                    "subtasks not blank"
                    (assertEqual
                         "Returns False"
                         False
                         (isBlank (Task "" Nothing (fromList [ST.new "One" True]) Nothing)))
              , testCase
                    "due date not blank"
                    (assertEqual
                         "Returns False"
                         False
                         (isBlank
                              (Task "" Nothing empty (DueDate <$> (fromGregorianValid 2018 05 18)))))
              ]
        ]
