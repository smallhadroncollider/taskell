{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module IO.Trello.List (
    List
  , cards
  , setCards
  , trelloListToList
) where

import ClassyPrelude

import Control.Lens (makeLenses, (&), (^.), (.~))

import IO.Trello.Aeson (deriveFromJSON)
import IO.Trello.Card (Card, cardToTask)
import qualified Data.Taskell.List as L (List, create)
import Data.Time.LocalTime (TimeZone)

data List = List {
    _name  :: Text
  , _cards :: [Card]
} deriving (Eq, Show)

-- create Aeson code
$(deriveFromJSON ''List)

-- create lenses
$(makeLenses ''List)


-- operations
setCards :: List -> [Card] -> List
setCards list cs = list & cards .~ cs

trelloListToList :: TimeZone -> List -> L.List
trelloListToList tz ls = L.create (ls ^. name) (fromList $ cardToTask tz <$> (ls ^. cards))
