{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.TaskTest (
    test_task
) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.Taskell.Task

testTask :: Task
testTask = Task {
    description = "Test"
  , summary = Nothing
  , subTasks = fromList [
        SubTask "One" True
      , SubTask "Two" False
      , SubTask "Three" False
    ]
  , due = Nothing
}

-- tests
test_task :: TestTree
test_task =
    testGroup "Data.Taskell.Task" [
        testCase "blank" (
            assertEqual
                "Returns empty task"
                (Task "" Nothing empty Nothing)
                blank
        )

      , testCase "new" (
            assertEqual
                "Returns a new task"
                (Task "Hello" Nothing empty Nothing)
                (new "Hello")
        )

      , testCase "blankSubTask" (
            assertEqual
                "Returns an empty sub-task"
                (SubTask "" False)
                blankSubTask
        )

      , testCase "subTask" (
            assertEqual
                "Returns a new sub-task"
                (SubTask "Hello" True)
                (subTask "Hello" True)
        )

      , testGroup "getSubTask" [
            testCase "exists" (
                assertEqual
                    "Returns a the sub-task"
                    (Just (SubTask "Two" False))
                    (getSubTask 1 testTask)
            )

          , testCase "doesn't exist" (
                assertEqual
                    "Returns Nothing"
                    Nothing
                    (getSubTask 10 testTask)
            )
        ]

      , testGroup "setSummary" [
            testCase "empty sub-tasks" (
                assertEqual
                    "Updates summary"
                    (Task "Test" (Just "Summary") empty Nothing)
                    (setSummary "Summary" (new "Test"))
            )
          , testCase "empty" (
                assertEqual
                    "Keeps summary as Nothing"
                    (new "Test")
                    (setSummary "" (new "Test"))
            )

          , testCase "blank" (
                assertEqual
                    "Keeps summary as Nothing"
                    (new "Test")
                    (setSummary "   " (new "Test"))
            )
        ]

      , testGroup "addSubTask" [
            testCase "existing" (
                assertEqual
                    "Returns the task with added subtask"
                    (Task "Test" Nothing (fromList [
                        SubTask "One" True
                      , SubTask "Two" False
                      , SubTask "Three" False
                      , SubTask "Four" True
                    ]) Nothing)
                    (addSubTask (SubTask "Four" True) testTask)
            )

          , testCase "empty sub-tasks" (
                assertEqual
                    "Returns the task with added subtask"
                    (Task "Test" Nothing (fromList [SubTask "One" False]) Nothing)
                    (addSubTask (SubTask "One" False) (new "Test"))
            )
        ]

      , testCase "setSubTaskName" (
            assertEqual
                "Returns updated subtask"
                (SubTask "New" False)
                (setSubTaskName "New" (SubTask "Original" False))
        )

      , testGroup "hasSubTasks" [
            testCase "existing" (
                assertEqual
                    "Returns True"
                    True
                    (hasSubTasks testTask)
            )

          , testCase "empty sub-tasks" (
                assertEqual
                    "Returns False"
                    False
                    (hasSubTasks (new "Test"))
            )
        ]

      , testGroup "updateSubTask" [
            testCase "exists" (
                assertEqual
                    "Returns updated task"
                    (Task "Test" Nothing (fromList [
                         SubTask "One" True
                       , SubTask "Cow" False
                       , SubTask "Three" False
                    ]) Nothing)
                    (updateSubTask 1 (\s -> s { name = "Cow" }) testTask)
            )

          , testCase "doesn't exist" (
                assertEqual
                    "Returns task"
                    (Task "Test" Nothing (fromList [
                         SubTask "One" True
                       , SubTask "Two" False
                       , SubTask "Three" False
                    ]) Nothing)
                    (updateSubTask 10 (\s -> s { name = "Cow" }) testTask)
            )
        ]

      , testGroup "removeSubTask" [
            testCase "exists" (
                assertEqual
                    "Returns updated task"
                    (Task "Test" Nothing (fromList [
                         SubTask "One" True
                       , SubTask "Three" False
                    ]) Nothing)
                    (removeSubTask 1 testTask)
            )

          , testCase "doesn't exist" (
                assertEqual
                    "Returns task"
                    (Task "Test" Nothing (fromList [
                         SubTask "One" True
                       , SubTask "Two" False
                       , SubTask "Three" False
                    ]) Nothing)
                    (removeSubTask 10 testTask)
            )
        ]

      , testGroup "toggleComplete" [
            testCase "complete" (
                assertEqual
                    "Marks sub-task as incomplete"
                    (SubTask "One" False)
                    (toggleComplete (SubTask "One" True))
            )

          , testCase "incomplete" (
                assertEqual
                    "Marks sub-task as complete"
                    (SubTask "One" True)
                    (toggleComplete (SubTask "One" False))
            )
        ]

      , testCase "countSubTasks" (
            assertEqual
                "Returns 3"
                3
                (countSubTasks testTask)
        )


      , testCase "countCompleteSubTasks" (
            assertEqual
                "Returns 1"
                1
                (countCompleteSubTasks testTask)
        )

      , testGroup "contains" [
            testCase "in task" (
                assertEqual
                    "Finds in task"
                    True
                    (contains "Test" testTask)
            )

          , testCase "in sub-task" (
                assertEqual
                    "Find sub-task"
                    True
                    (contains "One" testTask)
            )

          , testCase "missing" (
                assertEqual
                    "Find sub-task"
                    False
                    (contains "Fish" testTask)
            )
        ]

      , testGroup "isBlank" [
            testCase "blank" (
                assertEqual
                    "Return True"
                    True
                    (isBlank (Task "" Nothing empty Nothing))
            )

          , testCase "not blank" (
                assertEqual
                    "Returns False"
                    False
                    (isBlank testTask)
            )
        ]
    ]
