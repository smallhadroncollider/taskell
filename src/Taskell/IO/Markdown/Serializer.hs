{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.IO.Markdown.Serializer
    ( serialize
    , MarkdownInfo(MarkdownInfo)
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import qualified Data.Text as T (splitOn)

import Data.Time.Zones (TZ)

import           Taskell.Data.Date    (Due, timeToOutput, timeToOutputLocal)
import           Taskell.Data.List    (List, tasks, title)
import           Taskell.Data.Lists   (Lists)
import qualified Taskell.Data.Subtask as ST (Subtask, complete, name)
import qualified Taskell.Data.Task    as T (Task, description, due, name, subtasks)

import Taskell.IO.Config.Markdown (Config, descriptionOutput, dueOutput, localTimes, subtaskOutput,
                                   taskOutput, titleOutput)

data MarkdownInfo = MarkdownInfo
    { mdTZ     :: TZ
    , mdConfig :: Config
    }

type ReaderMarkdown = Reader MarkdownInfo

-- utility functions
askConf :: (Config -> a) -> ReaderMarkdown a
askConf fn = fn <$> asks mdConfig

strMay :: (Applicative m) => (a -> m Text) -> Maybe a -> m Text
strMay _ Nothing   = pure ""
strMay fn (Just a) = fn a

space :: Text -> Text -> Text
space symbol txt = symbol <> " " <> txt

timeFn :: ReaderMarkdown (Due -> Text)
timeFn = bool timeToOutput <$> (timeToOutputLocal <$> asks mdTZ) <*> askConf localTimes

-- serializers
subtaskCompleteS :: Bool -> Text
subtaskCompleteS True  = "[x]"
subtaskCompleteS False = "[ ]"

subtaskS :: ST.Subtask -> ReaderMarkdown Text
subtaskS st = do
    symbol <- askConf subtaskOutput
    pure $ unwords [symbol, subtaskCompleteS (st ^. ST.complete), st ^. ST.name]

subtasksS :: Seq ST.Subtask -> ReaderMarkdown Text
subtasksS sts = intercalate "\n" <$> sequence (subtaskS <$> sts)

descriptionS :: Text -> ReaderMarkdown Text
descriptionS desc = do
    symbol <- askConf descriptionOutput
    pure . intercalate "\n" $ space symbol <$> T.splitOn "\n" desc

dueS :: Due -> ReaderMarkdown Text
dueS due = do
    symbol <- askConf dueOutput
    fn <- timeFn
    pure $ space symbol (fn due)

nameS :: Text -> ReaderMarkdown Text
nameS desc = space <$> askConf taskOutput <*> pure desc

taskS :: T.Task -> ReaderMarkdown Text
taskS t =
    unlines . filter (/= "") <$>
    sequence
        [ nameS (t ^. T.name)
        , strMay dueS (t ^. T.due)
        , strMay descriptionS (t ^. T.description)
        , subtasksS (t ^. T.subtasks)
        ]

listS :: List -> ReaderMarkdown Text
listS list = do
    symbol <- askConf titleOutput
    taskString <- concat <$> sequence (taskS <$> list ^. tasks)
    pure . space symbol $ concat [list ^. title, "\n\n", taskString]

-- serialize
serialize :: Lists -> ReaderMarkdown Text
serialize ls = intercalate "\n" <$> sequence (listS <$> ls)
