module IO.Config.General
    ( Config
    , defaultConfig
    , parser
    , filename
    , debug
    ) where

import ClassyPrelude

import Data.Ini.Config

import IO.Config.Parser (noEmpty)

data Config = Config
    { filename :: FilePath
    , debug    :: Bool
    }

defaultConfig :: Config
defaultConfig = Config {filename = "taskell.md", debug = False}

filenameP :: SectionParser String
filenameP = maybe (filename defaultConfig) unpack . (noEmpty =<<) <$> fieldMb "filename"

debugP :: SectionParser Bool
debugP = fieldFlagDef "debug" False

parser :: IniParser Config
parser = fromMaybe defaultConfig <$> sectionMb "general" (Config <$> filenameP <*> debugP)
