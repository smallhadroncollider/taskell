module IO.Config.GitHub where

import ClassyPrelude

import Data.Ini.Config

import IO.HTTP.GitHub (GitHubToken)

data Config = Config
    { token :: Maybe GitHubToken
    }

defaultConfig :: Config
defaultConfig = Config {token = Nothing}

parser :: IniParser Config
parser =
    fromMaybe defaultConfig <$>
    sectionMb
        "github"
        (do tokenCf <- fieldMb "token"
            return Config {token = tokenCf})
