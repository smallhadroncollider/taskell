{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.List (
    List(..)
  , setCards
  , trelloListToList
) where

import ClassyPrelude

import Data.Aeson
import IO.Trello.Card (Card, cardToTask)
import qualified Data.Taskell.List as TL (List(..))
import Data.Time.LocalTime (TimeZone)

data List = List {
    id :: Text
  , name  :: Text
  , cards :: [Card]
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

setCards :: List -> [Card] -> List
setCards list cs = list { cards = cs }

trelloListToList :: TimeZone -> List -> TL.List
trelloListToList tz ls = TL.List {
        TL.title = name ls
      , TL.tasks = fromList $ cardToTask tz <$> cards ls
    }
