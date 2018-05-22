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

import Data.Taskell.Task (SubTask, subTask)

data ChecklistItem = ChecklistItem {
    name :: Text
  , state :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ChecklistWrapper = ChecklistWrapper {
    checkItems :: [ChecklistItem]
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

checklistItemToSubTask :: ChecklistItem -> SubTask
checklistItemToSubTask cl = subTask (name cl) (state cl == "complete")
