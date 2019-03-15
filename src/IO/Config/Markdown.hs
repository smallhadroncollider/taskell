module IO.Config.Markdown where

import ClassyPrelude

import Data.Ini.Config

import IO.Config.Parser (noEmpty, parseText)

data Config = Config
    { titleOutput       :: Text
    , taskOutput        :: Text
    , descriptionOutput :: Text
    , dueOutput         :: Text
    , subtaskOutput     :: Text
    }

defaultConfig :: Config
defaultConfig =
    Config
    { titleOutput = "##"
    , taskOutput = "-"
    , descriptionOutput = "    >"
    , dueOutput = "    @"
    , subtaskOutput = "    *"
    }

parser :: IniParser Config
parser =
    fromMaybe defaultConfig <$>
    sectionMb
        "markdown"
        (do titleOutputCf <-
                fromMaybe (titleOutput defaultConfig) . (noEmpty . parseText =<<) <$>
                fieldMb "title"
            taskOutputCf <-
                fromMaybe (taskOutput defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "task"
            descriptionOutputCf <-
                fromMaybe (descriptionOutput defaultConfig) . (noEmpty . parseText =<<) <$>
                fieldMb "summary"
            dueOutputCf <-
                fromMaybe (dueOutput defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "due"
            subtaskOutputCf <-
                fromMaybe (subtaskOutput defaultConfig) . (noEmpty . parseText =<<) <$>
                fieldMb "subtask"
            pure
                Config
                { titleOutput = titleOutputCf
                , taskOutput = taskOutputCf
                , descriptionOutput = descriptionOutputCf
                , dueOutput = dueOutputCf
                , subtaskOutput = subtaskOutputCf
                })
