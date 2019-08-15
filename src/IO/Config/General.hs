module IO.Config.General where

import ClassyPrelude

import Data.Ini.Config

import IO.Config.Parser (noEmpty)

data Config = Config
    { filename :: FilePath
    , debug    :: Bool
    }

defaultConfig :: Config
defaultConfig = Config {filename = "taskell.md", debug = False}

parser :: IniParser Config
parser =
    fromMaybe defaultConfig <$>
    sectionMb
        "general"
        (do filenameCf <-
                maybe (filename defaultConfig) unpack . (noEmpty =<<) <$> fieldMb "filename"
            debugCf <- fieldFlagDef "debug" False
            pure Config {filename = filenameCf, debug = debugCf})
