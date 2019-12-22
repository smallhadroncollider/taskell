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

import IO.HTTP.Aeson                (deriveFromJSON)
import IO.HTTP.Trello.ChecklistItem (ChecklistItem, checklistItemToSubTask)

import           Data.Taskell.Date (textToTime)
import qualified Data.Taskell.Task as T (Task, due, new, setDescription, subtasks)

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
cardToTask :: Card -> T.Task
cardToTask card =
    task & T.due .~ textToTime (fromMaybe "" (card ^. due)) & T.subtasks .~
    fromList (checklistItemToSubTask <$> fromMaybe [] (card ^. checklists))
  where
    task = T.setDescription (card ^. desc) $ T.new (card ^. name)

setChecklists :: Card -> [ChecklistItem] -> Card
setChecklists card cls = card & checklists .~ Just cls
