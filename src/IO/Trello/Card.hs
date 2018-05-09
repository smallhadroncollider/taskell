{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.Card (
    Card
  , cardToTask
) where

import ClassyPrelude

import Data.Aeson
import Data.Taskell.Task (Task, new)

data Card = Card {
    name :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

cardToTask :: Card -> Task
cardToTask = new . name
