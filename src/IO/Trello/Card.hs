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
import Data.Taskell.Date (utcToLocalDay)
import Data.Time.Format (parseTimeM, iso8601DateFormat)
import Data.Time.LocalTime (TimeZone)

data Card = Card {
    name :: Text
  , desc :: Text
  , due :: Maybe Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

textToTime :: TimeZone -> Text -> Maybe Day
textToTime tz text = utcToLocalDay tz <$> utc
    where utc = parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q%Z")) $ unpack text

cardToTask :: TimeZone -> Card -> T.Task
cardToTask tz card = task { T.due = textToTime tz $ fromMaybe "" (due card) }
    where task = T.setSummary (desc card)  $ T.new (name card)
