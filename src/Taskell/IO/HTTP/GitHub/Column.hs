{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.HTTP.GitHub.Column
    ( Column
    , columnToList
    , cardsURL
    ) where

import ClassyPrelude

import Control.Lens (Lens', makeLenses, (^.))

import Taskell.IO.HTTP.Aeson (deriveFromJSON)

import qualified Taskell.Data.List as L (List, create)
import qualified Taskell.Data.Task as T (Task)

data Column = Column
    { _name      :: Text
    , _cards_url :: Text
    } deriving (Eq, Show)

-- create Aeson code
$(deriveFromJSON ''Column)

-- create lenses
$(makeLenses ''Column)

-- operations
cardsURL :: Lens' Column Text
cardsURL = cards_url

columnToList :: Column -> [T.Task] -> L.List
columnToList ls tasks = L.create (ls ^. name) (fromList tasks)
