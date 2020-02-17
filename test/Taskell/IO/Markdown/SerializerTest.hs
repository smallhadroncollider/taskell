{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.Markdown.SerializerTest
    ( test_serializer
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens         ((&), (.~))
import Control.Monad.Reader (runReader)
import Data.FileEmbed       (embedFile)

import Data.Time.Zones     (utcTZ)
import Data.Time.Zones.All (TZLabel (America__New_York), tzByLabel)

import           Taskell.Data.Date              (textToTime)
import qualified Taskell.Data.List              as L (append, empty)
import           Taskell.Data.Lists             (Lists, appendToLast, newList)
import qualified Taskell.Data.Subtask           as ST (new)
import           Taskell.Data.Task              (Task, addSubtask, due, new, setDescription)
import           Taskell.IO.Config.Markdown     (Config (..), defaultConfig)
import           Taskell.IO.Markdown.Parser     (parse)
import           Taskell.IO.Markdown.Serializer (MarkdownInfo (MarkdownInfo), serialize)

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

lists :: Lists
lists = fromList [L.append task (L.empty "To Do"), L.append (new "Fish") (L.empty "Done")]

listWithItem :: Lists
listWithItem = appendToLast task list

makeSubTask :: Text -> Bool -> Lists
makeSubTask t b = appendToLast (addSubtask (ST.new t b) task) list

makeSubTasks :: [Text] -> Bool -> Lists
makeSubTasks ts b = appendToLast (foldr addSubtask task subtasks) list
  where
    subtasks = flip ST.new b <$> ts

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

taskWithDueTime :: Task
taskWithDueTime = task & due .~ textToTime "2018-04-12 12:00 UTC"

listWithDueTimeItem :: Lists
listWithDueTimeItem = appendToLast taskWithDueTime list

-- reader
defaultReader :: MarkdownInfo
defaultReader = MarkdownInfo utcTZ defaultConfig

alternativeReader :: MarkdownInfo
alternativeReader = MarkdownInfo utcTZ alternativeConfig

-- simplify tests
serialize' :: MarkdownInfo -> Lists -> Text
serialize' md ls = runReader (serialize ls) md

-- tests
test_serializer :: TestTree
test_serializer =
    testGroup
        "IO.Markdown"
        [ testGroup
              "Serialisation"
              [ testGroup
                    "Default Format"
                    [ testCase
                          "Standard lists"
                          (assertEqual
                               "Markdown formatted output"
                               "## To Do\n\n- Test Item\n\n## Done\n\n- Fish\n"
                               (serialize' defaultReader lists))
                    , testCase
                          "Standard list"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n- Test Item\n"
                               (serialize' defaultReader listWithItem))
                    , testCase
                          "Standard list with summary"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n- Test Item\n    > Summary\n"
                               (serialize' defaultReader listWithSummaryItem))
                    , testCase
                          "Standard list with date"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n- Test Item\n    @ 2018-04-12\n"
                               (serialize' defaultReader listWithDueDateItem))
                    , testCase
                          "Standard list with date - timezone"
                          (assertEqual
                               "Use UTC timezone"
                               "## Test\n\n- Test Item\n    @ 2018-04-12 12:00 UTC\n"
                               (serialize'
                                    (MarkdownInfo (tzByLabel America__New_York) defaultConfig)
                                    listWithDueTimeItem))
                    , testCase
                          "Standard list with sub-task"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n- Test Item\n    * [x] Blah\n"
                               (serialize' defaultReader (makeSubTask "Blah" True)))
                    , testCase
                          "Standard list with sub-tasks"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n- Test Item\n    * [x] Blah\n    * [x] Cow\n    * [x] Spoon\n"
                               (serialize'
                                    defaultReader
                                    (makeSubTasks ["Spoon", "Cow", "Blah"] True)))
                    , testCase
                          "Standard list with multi-line summary"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n- Test Item\n    > Summary Line 1\n    > Summary Line 2\n"
                               (serialize' defaultReader listWithMultiLineSummaryItem))
                    ]
              , testGroup
                    "Alternative Format"
                    [ testCase
                          "Standard list"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n### Test Item\n"
                               (serialize' alternativeReader listWithItem))
                    , testCase
                          "Standard list with sub-task"
                          (assertEqual
                               "Markdown formatted output"
                               "## Test\n\n### Test Item\n- [ ] Blah\n"
                               (serialize' alternativeReader (makeSubTask "Blah" False)))
                    , testCase
                          "Standard list with date - timezone"
                          (assertEqual
                               "Uses local timezone"
                               "## Test\n\n### Test Item\n@ 2018-04-12 08:00 EDT\n"
                               (serialize'
                                    (MarkdownInfo (tzByLabel America__New_York) alternativeConfig)
                                    listWithDueTimeItem))
                    ]
              ]
        , testCase
              "Parse then serialize"
              (assertEqual
                   "Returns same text"
                   (Right file)
                   (serialize' defaultReader <$> parse defaultConfig file))
        ]
