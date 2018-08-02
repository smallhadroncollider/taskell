module IO.Config.Trello where

import ClassyPrelude

import Data.Ini.Config

import IO.Trello (TrelloToken)

data Config = Config {
        token :: Maybe TrelloToken
    }

defaultConfig :: Config
defaultConfig = Config {
    token = Nothing
}

parser :: IniParser Config
parser = fromMaybe defaultConfig <$>
    sectionMb "trello" (do
        tokenCf <- fieldMb "token"
        return Config { token = tokenCf }
    )

