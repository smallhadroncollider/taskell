{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.List (
    List(..)
  , trelloListToList
) where

import ClassyPrelude

import Data.Aeson
import IO.Trello.Card (Card, cardToTask)
import qualified Data.Taskell.List as TL (List(..))

data List = List {
    id :: Text
  , name  :: Text
  , cards :: [Card]
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

trelloListToList :: List -> TL.List
trelloListToList ls = TL.List {
        TL.title = name ls
      , TL.tasks = fromList $ cardToTask <$> cards ls
    }
