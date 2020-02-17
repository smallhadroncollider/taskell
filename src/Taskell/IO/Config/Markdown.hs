module Taskell.IO.Config.Markdown
    ( Config(Config)
    , defaultConfig
    , parser
    , titleOutput
    , taskOutput
    , descriptionOutput
    , dueOutput
    , subtaskOutput
    , localTimes
    ) where

import ClassyPrelude

import Data.Ini.Config

import Taskell.IO.Config.Parser (noEmpty, parseText)

data Config = Config
    { titleOutput       :: Text
    , taskOutput        :: Text
    , descriptionOutput :: Text
    , dueOutput         :: Text
    , subtaskOutput     :: Text
    , localTimes        :: Bool
    }

defaultConfig :: Config
defaultConfig =
    Config
    { titleOutput = "##"
    , taskOutput = "-"
    , descriptionOutput = "    >"
    , dueOutput = "    @"
    , subtaskOutput = "    *"
    , localTimes = False
    }

titleOutputP :: SectionParser Text
titleOutputP = fromMaybe (titleOutput defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "title"

taskOutputP :: SectionParser Text
taskOutputP = fromMaybe (taskOutput defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "task"

descriptionOutputP :: SectionParser Text
descriptionOutputP =
    fromMaybe (descriptionOutput defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "summary"

dueOutputP :: SectionParser Text
dueOutputP = fromMaybe (dueOutput defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "due"

subtaskOutputP :: SectionParser Text
subtaskOutputP =
    fromMaybe (subtaskOutput defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "subtask"

localTimesP :: SectionParser Bool
localTimesP = fieldFlagDef "localTimes" (localTimes defaultConfig)

parser :: IniParser Config
parser =
    fromMaybe defaultConfig <$>
    sectionMb
        "markdown"
        (Config <$> titleOutputP <*> taskOutputP <*> descriptionOutputP <*> dueOutputP <*>
         subtaskOutputP <*>
         localTimesP)
