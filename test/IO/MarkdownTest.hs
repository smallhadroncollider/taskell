{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.MarkdownTest (
    test_markdown
) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (ignoreTest)

import IO.Markdown.Internal (start, parse, listStringify)
import IO.Config (MarkdownConfig(..), defaultMarkdownConfig, defaultConfig)
import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.Task (Task, new, addSubtask, setDescription, setDue)
import qualified Data.Taskell.Subtask as ST (new)

-- alternative markdown configs
alternativeMarkdownConfig :: MarkdownConfig
alternativeMarkdownConfig = MarkdownConfig {
    titleOutput = "##",
    taskOutput = "###",
    descriptionOutput = ">",
    dueOutput = "@",
    subtaskOutput = "-"
}

-- useful records
task :: Task
task = new "Test Item"

list :: Lists
list = newList "Test" empty

listWithItem :: Lists
listWithItem = appendToLast task list

makeSubTask :: Text -> Bool -> Lists
makeSubTask t b = appendToLast (addSubtask (ST.new t b) task) list

taskWithSummary :: Task
taskWithSummary = setDescription "Summary" task

taskWithMultiLineSummary :: Task
taskWithMultiLineSummary = setDescription "Summary Line 1\nSummary Line 2" task

listWithSummaryItem :: Lists
listWithSummaryItem = appendToLast taskWithSummary list

listWithMultiLineSummaryItem :: Lists
listWithMultiLineSummaryItem = appendToLast taskWithMultiLineSummary list

taskWithDueDate :: Task
taskWithDueDate = setDue "2018-04-12" task

listWithDueDateItem :: Lists
listWithDueDateItem = appendToLast taskWithDueDate list

-- tests
test_markdown :: TestTree
test_markdown =
    testGroup "IO.Markdown" [
        testGroup "Line Parsing" [
            testGroup "Default Format" [
                testCase "List Title" (
                    assertEqual
                        "One list"
                        (list, [])
                        (start defaultMarkdownConfig (empty, []) ("## Test", 1))
                )

              , testCase "Blank line" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start defaultMarkdownConfig (empty, []) ("", 1))
                )

              , testCase "Just spaces" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start defaultMarkdownConfig (empty, []) ("        ", 1))
                )

              , testCase "Error" (
                    assertEqual
                        "Error on line 1"
                        (empty, [1])
                        (start defaultMarkdownConfig (empty, []) ("Test", 1))
                )

              , testCase "List item" (
                    assertEqual
                        "List item"
                        (listWithItem, [])
                        (start defaultMarkdownConfig (list, []) ("- Test Item", 1))
                )

              , testCase "Summary" (
                    assertEqual
                        "Summary"
                        (listWithSummaryItem, [])
                        (start defaultMarkdownConfig (listWithItem, []) ("    > Summary", 1))
                )

              , testCase "Due Date" (
                    assertEqual
                        "Due Date"
                        (listWithDueDateItem, [])
                        (start defaultMarkdownConfig (listWithItem, []) ("    @ 2018-04-12", 1))
                )

              , testCase "Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (makeSubTask "Blah" False, [])
                        (
                            start
                                defaultMarkdownConfig
                                (listWithItem, [])
                                ("    * Blah", 1)
                        )
                )

              , testCase "Complete Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (makeSubTask "Blah" True, [])
                        (start defaultMarkdownConfig (listWithItem, []) ("    * ~Blah~", 1))
                )

              , ignoreTest $ testCase "List item without list" (
                    assertEqual
                        "Parse Error"
                        (empty, [1])
                        (start defaultMarkdownConfig (empty, []) ("- Test Item", 1))
                )

              , ignoreTest $ testCase "Sub task without list item" (
                    assertEqual
                        "Parse Error"
                        (list, [1])
                        (start defaultMarkdownConfig (list, []) ("    * Blah", 1))
                )
            ]

          , testGroup "Alternative Format" [
                testCase "List Title" (
                    assertEqual
                        "One list"
                        (list, [])
                        (start alternativeMarkdownConfig (empty, []) ("## Test", 1))
                )

              , testCase "Blank line" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start alternativeMarkdownConfig (empty, []) ("", 1))
                )

              , testCase "Just spaces" (
                    assertEqual
                        "Nothing"
                        (empty, [])
                        (start alternativeMarkdownConfig (empty, []) ("        ", 1))
                )

              , testCase "Error" (
                    assertEqual
                        "Error on line 1"
                        (empty, [1])
                        (start alternativeMarkdownConfig (empty, []) ("* Test", 1))
                )

              , testCase "List item" (
                    assertEqual
                        "List item"
                        (listWithItem, [])
                        (start alternativeMarkdownConfig (list, []) ("### Test Item", 1))
                )

              , testCase "Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (makeSubTask "Blah" False, [])
                        (start alternativeMarkdownConfig (listWithItem, []) ("- [ ] Blah", 1))
                )

              , testCase "Complete Sub-Task" (
                    assertEqual
                        "List item with Sub-Task"
                        (makeSubTask "Blah" True, [])
                        (start alternativeMarkdownConfig (listWithItem, []) ("- [x] Blah", 1))
                )

              , testCase "Blank Sub-Task" (
                    assertEqual
                        "List item with blank Sub-Task"
                        (makeSubTask "" True, [])
                        (start alternativeMarkdownConfig (listWithItem, []) ("- [x] ", 1))
                )

              , testCase "Sub-Task (old style)" (
                    assertEqual
                        "List item with Sub-Task"
                        (makeSubTask "Blah" False, [])
                        (start alternativeMarkdownConfig (listWithItem, []) ("- Blah", 1))
                )

              , testCase "Complete Sub-Task (old style)" (
                    assertEqual
                        "List item with Sub-Task"
                        (makeSubTask "Blah" True, [])
                        (start alternativeMarkdownConfig (listWithItem, []) ("- ~Blah~", 1))
                )
            ]
        ]

      , testGroup "Parsing" [
            testCase "List Title" (
                assertEqual
                    "One empty list"
                    (Right list)
                    (parse defaultConfig (encodeUtf8 "## Test"))
            )

          , testCase "List Items" (
                assertEqual
                    "List with item"
                    (Right listWithItem)
                    (parse defaultConfig (encodeUtf8 "## Test\n- Test Item"))
            )

          , testCase "List Item with Summary" (
                assertEqual
                    "List item with a summary"
                    (Right listWithSummaryItem)
                    (parse defaultConfig (encodeUtf8 "## Test\n- Test Item\n    > Summary"))
            )

          , testCase "Parsing Errors" (
                assertEqual
                    "Errors"
                    (Left "could not parse line(s) 3, 5")
                    (parse defaultConfig (encodeUtf8 "## Test\n- Test Item\n* Spoon\n- Test Item\nCow"))
            )

          , testCase "List Item with multi-line Summary" (
                assertEqual
                    "List item with a summary"
                    (Right listWithMultiLineSummaryItem)
                    (parse defaultConfig (encodeUtf8 "## Test\n- Test Item\n    > Summary Line 1\n    > Summary Line 2"))
            )
        ]

      , testGroup "Stringification" [
            testGroup "Default Format" [
                testCase "Standard list" (
                    assertEqual
                        "Markdown formatted output"
                        "## Test\n\n- Test Item\n"
                        (foldl' (listStringify defaultMarkdownConfig) "" listWithItem)
                )

              , testCase "Standard list with summary" (
                    assertEqual
                        "Markdown formatted output"
                        "## Test\n\n- Test Item\n    > Summary\n"
                        (foldl' (listStringify defaultMarkdownConfig) "" listWithSummaryItem)
                )

              , testCase "Standard list with date" (
                    assertEqual
                        "Markdown formatted output"
                        "## Test\n\n- Test Item\n    @ 2018-04-12\n"
                        (foldl' (listStringify defaultMarkdownConfig) "" listWithDueDateItem)
                )

              , testCase "Standard list with sub-task" (
                    assertEqual
                        "Markdown formatted output"
                        "## Test\n\n- Test Item\n    * [x] Blah\n"
                        (foldl' (listStringify defaultMarkdownConfig) "" (makeSubTask "Blah" True))
                )

              , testCase "Standard list with multi-line summary" (
                    assertEqual
                        "Markdown formatted output"
                        "## Test\n\n- Test Item\n    > Summary Line 1\n    > Summary Line 2\n"
                        (foldl' (listStringify defaultMarkdownConfig) "" listWithMultiLineSummaryItem)
                )

            ],

            testGroup "Alternative Format" [
                testCase "Standard list" (
                    assertEqual
                        "Markdown formatted output"
                        "## Test\n\n### Test Item\n"
                        (foldl' (listStringify alternativeMarkdownConfig) "" listWithItem)
                )

              , testCase "Standard list with sub-task" (
                    assertEqual
                        "Markdown formatted output"
                        "## Test\n\n- Test Item\n    * [ ] Blah\n"
                        (foldl' (listStringify defaultMarkdownConfig) "" (makeSubTask "Blah" False))
                )
            ]
        ]
    ]
