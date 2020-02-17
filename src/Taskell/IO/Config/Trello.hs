module Taskell.IO.Config.Trello
    ( Config
    , defaultConfig
    , parser
    , token
    ) where

import ClassyPrelude

import Data.Ini.Config

import Taskell.IO.HTTP.Trello (TrelloToken)

data Config = Config
    { token :: Maybe TrelloToken
    }

defaultConfig :: Config
defaultConfig = Config {token = Nothing}

tokenP :: SectionParser (Maybe TrelloToken)
tokenP = fieldMb "token"

parser :: IniParser Config
parser = fromMaybe defaultConfig <$> sectionMb "trello" (Config <$> tokenP)
