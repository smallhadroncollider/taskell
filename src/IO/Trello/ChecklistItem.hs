{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.ChecklistItem (
    ChecklistItem
  , checklistItemToSubTask
  , checkItems
) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.))
import Data.Aeson
import qualified Data.Taskell.Subtask as ST (Subtask, new)

data ChecklistItem = ChecklistItem {
    _name :: Text
  , _state :: Text
} deriving (Eq, Show, Generic)

-- strip underscores from field labels
instance FromJSON ChecklistItem where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- create lenses
$(makeLenses ''ChecklistItem)


-- operations
checklistItemToSubTask :: ChecklistItem -> ST.Subtask
checklistItemToSubTask cl = ST.new (cl ^. name) ((cl ^. state) == "complete")


-- check list wrapper
newtype ChecklistWrapper = ChecklistWrapper {
    _checkItems :: [ChecklistItem]
} deriving (Eq, Show, Generic)

-- strip underscores from field labels
instance FromJSON ChecklistWrapper where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- create lenses
$(makeLenses ''ChecklistWrapper)
