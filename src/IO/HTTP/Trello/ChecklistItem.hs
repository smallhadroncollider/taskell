{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IO.HTTP.Trello.ChecklistItem
    ( ChecklistItem
    , checklistItemToSubTask
    , checkItems
    ) where

import ClassyPrelude

import           Control.Lens         (makeLenses, (^.))
import qualified Data.Taskell.Subtask as ST (Subtask, new)
import           IO.HTTP.Aeson        (deriveFromJSON)

data ChecklistItem = ChecklistItem
    { _name  :: Text
    , _state :: Text
    } deriving (Eq, Show)

-- create Aeson code
$(deriveFromJSON ''ChecklistItem)

-- create lenses
$(makeLenses ''ChecklistItem)

-- operations
checklistItemToSubTask :: ChecklistItem -> ST.Subtask
checklistItemToSubTask cl = ST.new (cl ^. name) ((cl ^. state) == "complete")

-- check list wrapper
newtype ChecklistWrapper = ChecklistWrapper
    { _checkItems :: [ChecklistItem]
    } deriving (Eq, Show)

-- create Aeson code
$(deriveFromJSON ''ChecklistWrapper)

-- create lenses
$(makeLenses ''ChecklistWrapper)
