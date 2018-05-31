{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.Card (
    Card
  , idChecklists
  , cardToTask
  , setChecklists
) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.), (&), (.~))

import Data.Aeson
import qualified Data.Taskell.Task as T (Task, new, setDescription, due, subtasks)
import Data.Taskell.Date (utcToLocalDay)
import Data.Time.Format (parseTimeM, iso8601DateFormat)
import Data.Time.LocalTime (TimeZone)
import IO.Trello.Aeson (stripLensPrefix)
import IO.Trello.ChecklistItem (ChecklistItem, checklistItemToSubTask)

data Card = Card {
    _name :: Text
  , _desc :: Text
  , _due :: Maybe Text
  , _idChecklists :: [Text]
  , _checklists :: Maybe [ChecklistItem]
} deriving (Eq, Show, Generic)

-- strip underscores from field labels
instance FromJSON Card where
    parseJSON = stripLensPrefix

-- create lenses
$(makeLenses ''Card)


-- operations
textToTime :: TimeZone -> Text -> Maybe Day
textToTime tz text = utcToLocalDay tz <$> utc
    where utc = parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q%Z")) $ unpack text

cardToTask :: TimeZone -> Card -> T.Task
cardToTask tz card =
    task & T.due .~ textToTime tz (fromMaybe "" (card ^. due))
         & T.subtasks .~ fromList (checklistItemToSubTask <$> fromMaybe [] (card ^. checklists))
    where task = T.setDescription (card ^. desc) $ T.new (card ^. name)

setChecklists :: Card -> [ChecklistItem] -> Card
setChecklists card cls = card & checklists .~ Just cls
