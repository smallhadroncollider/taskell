{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.HTTP.Trello.Card
    ( Card
    , idChecklists
    , cardToTask
    , setChecklists
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (&), (.~), (^.))

import Taskell.IO.HTTP.Aeson                (deriveFromJSON)
import Taskell.IO.HTTP.Trello.ChecklistItem (ChecklistItem, checklistItemToSubTask)

import           Taskell.Data.Date (isoToTime)
import qualified Taskell.Data.Task as T (Task, due, new, setDescription, subtasks)

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
    task & T.due .~ isoToTime (fromMaybe "" (card ^. due)) & T.subtasks .~
    fromList (checklistItemToSubTask <$> fromMaybe [] (card ^. checklists))
  where
    task = T.setDescription (card ^. desc) $ T.new (card ^. name)

setChecklists :: Card -> [ChecklistItem] -> Card
setChecklists card cls = card & checklists .~ Just cls
