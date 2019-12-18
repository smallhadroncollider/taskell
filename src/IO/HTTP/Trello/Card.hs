{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IO.HTTP.Trello.Card
    ( Card
    , idChecklists
    , cardToTask
    , setChecklists
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (&), (.~), (^.))

import qualified Data.Taskell.Task as T (Task, due, new, setDescription, subtasks)
import           Data.Time.Format  (iso8601DateFormat, parseTimeM)

import IO.HTTP.Aeson                (deriveFromJSON)
import IO.HTTP.Trello.ChecklistItem (ChecklistItem, checklistItemToSubTask)

data Card = Card
    { _name         :: Text
    , _desc         :: Text
    , _due          :: Maybe Text
    , _idChecklists :: [Text]
    , _checklists   :: Maybe [ChecklistItem]
    } deriving (Eq, Show)

-- strip underscores from field labels
$(deriveFromJSON ''Card)

-- create lenses
$(makeLenses ''Card)

-- operations
textToTime :: Text -> Maybe UTCTime
textToTime = parseTimeM False defaultTimeLocale format . unpack
  where
    format = iso8601DateFormat (Just "%H:%M:%S%Q%Z")

cardToTask :: Card -> T.Task
cardToTask card =
    task & T.due .~ textToTime (fromMaybe "" (card ^. due)) & T.subtasks .~
    fromList (checklistItemToSubTask <$> fromMaybe [] (card ^. checklists))
  where
    task = T.setDescription (card ^. desc) $ T.new (card ^. name)

setChecklists :: Card -> [ChecklistItem] -> Card
setChecklists card cls = card & checklists .~ Just cls
