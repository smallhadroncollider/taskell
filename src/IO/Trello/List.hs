{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.List (
    List
  , cards
  , setCards
  , trelloListToList
) where

import ClassyPrelude

import Control.Lens (makeLenses, (&), (^.), (.~))

import Data.Aeson
import IO.Trello.Card (Card, cardToTask)
import qualified Data.Taskell.List as L (List, create)
import Data.Time.LocalTime (TimeZone)

data List = List {
    _name  :: Text
  , _cards :: [Card]
} deriving (Eq, Show, Generic)

-- strip underscores from field labels
instance FromJSON List where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- create lenses
$(makeLenses ''List)


-- operations
setCards :: List -> [Card] -> List
setCards list cs = list & cards .~ cs

trelloListToList :: TimeZone -> List -> L.List
trelloListToList tz ls = L.create (ls ^. name) (fromList $ cardToTask tz <$> (ls ^. cards))
