{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.Card (
    Card
  , idChecklists
  , cardToTask
  , setChecklists
) where

import ClassyPrelude

import Control.Lens ((&), (.~))

import Data.Aeson
import qualified Data.Taskell.Task as T (Task, new, setDescription, due, subtasks)
import Data.Taskell.Date (utcToLocalDay)
import Data.Time.Format (parseTimeM, iso8601DateFormat)
import Data.Time.LocalTime (TimeZone)
import IO.Trello.ChecklistItem (ChecklistItem, checklistItemToSubTask)

data Card = Card {
    name :: Text
  , desc :: Text
  , due :: Maybe Text
  , idChecklists :: [Text]
  , checklists :: Maybe [ChecklistItem]
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

textToTime :: TimeZone -> Text -> Maybe Day
textToTime tz text = utcToLocalDay tz <$> utc
    where utc = parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q%Z")) $ unpack text

cardToTask :: TimeZone -> Card -> T.Task
cardToTask tz card =
    task & T.due .~ textToTime tz (fromMaybe "" (due card))
         & T.subtasks .~ fromList (checklistItemToSubTask <$> fromMaybe [] (checklists card))
    where task = T.setDescription (desc card) $ T.new (name card)

setChecklists :: Card -> [ChecklistItem] -> Card
setChecklists card cls = card { checklists = Just cls }
