module IO.Config.General where

import ClassyPrelude

import Data.Ini.Config

import IO.Config.Parser (noEmpty)

data Config = Config {
        filename :: FilePath
    }

defaultConfig :: Config
defaultConfig = Config {
    filename = "taskell.md"
}

parser :: IniParser Config
parser = fromMaybe defaultConfig <$>
    sectionMb "general" (do
        filenameCf <- maybe (filename defaultConfig) unpack . (noEmpty =<<) <$> fieldMb "filename"
        return Config { filename = filenameCf }
    )
