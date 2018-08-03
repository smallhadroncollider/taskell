{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module IO.HTTP.GitHub.Column (
    Column
  , columnToList
  , cardsURL
) where

import ClassyPrelude

import Control.Lens (Lens', makeLenses, (^.))

import IO.HTTP.Aeson (deriveFromJSON)
import IO.HTTP.GitHub.Card (Card, cardToTask)

import qualified Data.Taskell.List as L (List, create)

data Column = Column {
    _name  :: Text
  , _cards_url :: Text
} deriving (Eq, Show)

-- create Aeson code
$(deriveFromJSON ''Column)

-- create lenses
$(makeLenses ''Column)


-- operations
cardsURL :: Lens' Column Text
cardsURL = cards_url

columnToList :: Column -> [Card] -> L.List
columnToList ls cards = L.create (ls ^. name) (fromList $ cardToTask <$> cards)
