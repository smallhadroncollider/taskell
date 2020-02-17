{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.Markdown.ParserTest
    ( test_parser
    ) where

import ClassyPrelude

import Test.Tasty

import Test.Tasty.HUnit

import Control.Lens   ((&), (.~))
import Data.FileEmbed (embedFile)

import Taskell.Data.Date  (textToTime)
import Taskell.Data.List  (create)
import Taskell.Data.Lists (Lists, analyse, appendToLast, newList)

import qualified Taskell.Data.Subtask       as ST (new)
import           Taskell.Data.Task          (Task, addSubtask, due, new, setDescription)
import           Taskell.IO.Config.Markdown (Config (Config), defaultConfig, descriptionOutput,
                                             dueOutput, localTimes, subtaskOutput, taskOutput,
                                             titleOutput)
import           Taskell.IO.Markdown.Parser (parse)

-- error message
err :: Either Text Lists
err = Left "Could not parse file."

-- complete taskell file
file :: Text
file = decodeUtf8 $(embedFile "test/Taskell/IO/data/roadmap.md")

-- alternative markdown configs
alternativeConfig :: Config
alternativeConfig =
    Config
    { titleOutput = "##"
    , taskOutput = "###"
    , descriptionOutput = ">"
    , dueOutput = "@"
    , subtaskOutput = "-"
    , localTimes = True
    }

-- useful records
task :: Task
task = new "Test Item"

list :: Lists
list = newList "Test" empty

listWithItem :: Lists
listWithItem = appendToLast task list

multiList :: Lists
multiList =
    fromList
        [create "Test" (fromList [new "Test Item"]), create "Test 2" (fromList [new "Test Item 2"])]

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
taskWithDueDate = task & due .~ textToTime "2018-04-12"

listWithDueDateItem :: Lists
listWithDueDateItem = appendToLast taskWithDueDate list

-- tests
test_parser :: TestTree
test_parser =
    testGroup
        "IO.Markdown"
        [ testGroup
              "Default Format"
              [ testCase
                    "List Title"
                    (assertEqual "One list" (Right list) (parse defaultConfig "## Test"))
              , testCase
                    "List item"
                    (assertEqual
                         "List item"
                         (Right listWithItem)
                         (parse defaultConfig "## Test\n\n- Test Item"))
              , testCase
                    "Multiple Lists"
                    (assertEqual
                         "List item"
                         (Right multiList)
                         (parse defaultConfig "## Test\n\n- Test Item\n\n## Test 2\n\n- Test Item 2"))
              , testCase
                    "Summary"
                    (assertEqual
                         "Summary"
                         (Right listWithSummaryItem)
                         (parse defaultConfig "## Test\n\n- Test Item\n    > Summary"))
              , testCase
                    "List Item with multi-line Summary"
                    (assertEqual
                         "List item with a summary"
                         (Right listWithMultiLineSummaryItem)
                         (parse
                              defaultConfig
                              "## Test\n\n- Test Item\n    > Summary Line 1\n    > Summary Line 2"))
              , testCase
                    "Due Date"
                    (assertEqual
                         "Due Date"
                         (Right listWithDueDateItem)
                         (parse defaultConfig "## Test\n\n- Test Item\n    @ 2018-04-12"))
              , testCase
                    "Sub-Task"
                    (assertEqual
                         "List item with Sub-Task"
                         (Right (makeSubTask "Blah" False))
                         (parse defaultConfig "## Test\n\n- Test Item\n    * [ ] Blah"))
              , testCase
                    "Complete Sub-Task"
                    (assertEqual
                         "List item with Sub-Task"
                         (Right (makeSubTask "Blah" True))
                         (parse defaultConfig "## Test\n\n- Test Item\n    * [x] Blah"))
              ]
        , testGroup
              "Alternative Format"
              [ testCase
                    "List Title"
                    (assertEqual "One list" (Right list) (parse alternativeConfig "## Test"))
              , testCase
                    "List item"
                    (assertEqual
                         "List item"
                         (Right listWithItem)
                         (parse alternativeConfig "## Test\n\n### Test Item"))
              , testCase
                    "Multiple Lists"
                    (assertEqual
                         "List item"
                         (Right multiList)
                         (parse
                              alternativeConfig
                              "## Test\n\n### Test Item\n\n## Test 2\n\n### Test Item 2"))
              , testCase
                    "Summary"
                    (assertEqual
                         "Summary"
                         (Right listWithSummaryItem)
                         (parse alternativeConfig "## Test\n\n### Test Item\n> Summary"))
              , testCase
                    "List Item with multi-line Summary"
                    (assertEqual
                         "List item with a summary"
                         (Right listWithMultiLineSummaryItem)
                         (parse
                              alternativeConfig
                              "## Test\n\n### Test Item\n> Summary Line 1\n> Summary Line 2"))
              , testCase
                    "Due Date"
                    (assertEqual
                         "Due Date"
                         (Right listWithDueDateItem)
                         (parse alternativeConfig "## Test\n\n### Test Item\n@ 2018-04-12"))
              , testCase
                    "Sub-Task"
                    (assertEqual
                         "List item with Sub-Task"
                         (Right (makeSubTask "Blah" False))
                         (parse alternativeConfig "## Test\n\n### Test Item\n- [ ] Blah"))
              , testCase
                    "Complete Sub-Task"
                    (assertEqual
                         "List item with Sub-Task"
                         (Right (makeSubTask "Blah" True))
                         (parse alternativeConfig "## Test\n\n### Test Item\n- [x] Blah"))
              ]
        , testGroup
              "Errors"
              [ testCase "Blank line" (assertEqual "Parse Error" err (parse defaultConfig ""))
              , testCase
                    "Just spaces"
                    (assertEqual "Parse Error" err (parse defaultConfig "        "))
              , testCase
                    "Just whitespace"
                    (assertEqual "Parse Error" err (parse defaultConfig "  \n  \n \t    "))
              , testCase "Error" (assertEqual "Parse Error" err (parse defaultConfig "Test"))
              , testCase
                    "List item without list"
                    (assertEqual "Parse Error" err (parse defaultConfig "- Test Item"))
              , testCase
                    "Sub task without list item"
                    (assertEqual "Parse Error" err (parse defaultConfig "## Test\n    * Blah"))
              ]
        , testCase
              "File"
              (assertEqual
                   "Parses whole file"
                   (Right "test\nLists: 6\nTasks: 202")
                   (analyse "test" <$> parse defaultConfig file))
        ]
