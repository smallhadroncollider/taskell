{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.Card (
    Card
  , cardToTask
) where

import ClassyPrelude

import Data.Aeson
import qualified Data.Taskell.Task as T (Task, new, setSummary, due)
import Data.Time.Format (parseTimeM, iso8601DateFormat)

data Card = Card {
    name :: Text
  , desc :: Text
  , due :: Maybe Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

textToTime :: Text -> Maybe Day
textToTime = (utctDay <$>) . parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q%Z")) . unpack

cardToTask :: Card -> T.Task
cardToTask card = task { T.due = textToTime $ fromMaybe "" (due card) }
    where task = T.setSummary (desc card)  $ T.new (name card)
