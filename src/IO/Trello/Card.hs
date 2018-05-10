{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.Card (
    Card
  , cardToTask
) where

import ClassyPrelude

import Data.Aeson
import Data.Taskell.Task (Task, new, setSummary)

data Card = Card {
    name :: Text
  , desc :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

cardToTask :: Card -> Task
cardToTask card = setSummary (desc card)  $ new (name card)
