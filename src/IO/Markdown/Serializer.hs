{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Markdown.Serializer where

import ClassyPrelude

import Control.Lens ((^.))

import qualified Data.Text as T (splitOn)

import Data.Time.Zones (TZ)

import           Data.Taskell.Date    (Due, timeToOutput, timeToOutputLocal)
import           Data.Taskell.List    (List, tasks, title)
import           Data.Taskell.Lists   (Lists)
import qualified Data.Taskell.Subtask as ST (Subtask, complete, name)
import qualified Data.Taskell.Task    as T (Task, description, due, name, subtasks)

import IO.Config.Markdown (Config, descriptionOutput, dueOutput, localTimes, subtaskOutput,
                           taskOutput, titleOutput)

data MarkdownInfo = MarkdownInfo
    { mdTZ     :: TZ
    , mdConfig :: Config
    }

type ReaderMarkdown = Reader MarkdownInfo Text

subtaskSymbol :: Bool -> Text
subtaskSymbol True  = "[x]"
subtaskSymbol False = "[ ]"

subtaskStringify :: ST.Subtask -> ReaderMarkdown
subtaskStringify st = do
    symbol <- subtaskOutput <$> asks mdConfig
    pure . concat $ [symbol, " ", subtaskSymbol (st ^. ST.complete), " ", st ^. ST.name]

descriptionStringify :: Text -> ReaderMarkdown
descriptionStringify desc = do
    symbol <- descriptionOutput <$> asks mdConfig
    let add d = concat [symbol, " ", d]
    pure . intercalate "\n" $ add <$> T.splitOn "\n" desc

dueStringify :: Due -> ReaderMarkdown
dueStringify time = do
    symbol <- dueOutput <$> asks mdConfig
    useLocal <- localTimes <$> asks mdConfig
    tz <- asks mdTZ
    let fn =
            if useLocal
                then timeToOutputLocal tz
                else timeToOutput
    pure $ concat [symbol, " ", fn time]

nameStringify :: Text -> ReaderMarkdown
nameStringify desc = do
    symbol <- taskOutput <$> asks mdConfig
    pure $ concat [symbol, " ", desc]

taskStringify :: T.Task -> ReaderMarkdown
taskStringify t = do
    nameString <- nameStringify (t ^. T.name)
    dueString <- fromMaybe "" <$> sequence (dueStringify <$> t ^. T.due)
    descriptionString <- fromMaybe "" <$> sequence (descriptionStringify <$> t ^. T.description)
    subtaskString <- intercalate "\n" <$> sequence (subtaskStringify <$> t ^. T.subtasks)
    pure . unlines . filter (/= "") $ [nameString, dueString, descriptionString, subtaskString]

listStringify :: List -> ReaderMarkdown
listStringify list = do
    symbol <- titleOutput <$> asks mdConfig
    taskString <- concat <$> sequence (taskStringify <$> list ^. tasks)
    pure $ concat [symbol, " ", list ^. title, "\n\n", taskString]

serialize :: Lists -> ReaderMarkdown
serialize ls = intercalate "\n" <$> sequence (listStringify <$> ls)
