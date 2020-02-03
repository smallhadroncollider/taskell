module IO.Config.GitHub
    ( Config
    , defaultConfig
    , parser
    , token
    ) where

import ClassyPrelude

import Data.Ini.Config

import IO.HTTP.GitHub (GitHubToken)

data Config = Config
    { token :: Maybe GitHubToken
    }

defaultConfig :: Config
defaultConfig = Config {token = Nothing}

tokenP :: SectionParser (Maybe GitHubToken)
tokenP = fieldMb "token"

parser :: IniParser Config
parser = fromMaybe defaultConfig <$> sectionMb "github" (Config <$> tokenP)
