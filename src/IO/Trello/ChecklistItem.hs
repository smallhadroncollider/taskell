{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module IO.Trello.ChecklistItem (
    ChecklistItem
  , checklistItemToSubTask
  , checkItems
) where

import ClassyPrelude

import Data.Aeson

import qualified Data.Taskell.Subtask as ST (Subtask, new)

data ChecklistItem = ChecklistItem {
    name :: Text
  , state :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ChecklistWrapper = ChecklistWrapper {
    checkItems :: [ChecklistItem]
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

checklistItemToSubTask :: ChecklistItem -> ST.Subtask
checklistItemToSubTask cl = ST.new (name cl) (state cl == "complete")
