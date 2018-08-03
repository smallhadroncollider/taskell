{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IO.HTTP.GitHub.Card (
    Card
  , cardToTask
) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.))

import Data.Text (replace)

import qualified Data.Taskell.Task as T (Task, new)
import IO.HTTP.Aeson (deriveFromJSON)

data Card = Card {
    _note :: Text
} deriving (Eq, Show)

-- strip underscores from field labels
$(deriveFromJSON ''Card)

-- create lenses
$(makeLenses ''Card)


-- operations
cardToTask :: Card -> T.Task
cardToTask card = T.new $ replace "\r" "" $ replace "\n" " " (card ^. note)
