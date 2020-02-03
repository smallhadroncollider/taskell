{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.HTTP.GitHub.Card
    ( MaybeCard(MaybeCard)
    , maybeCardToTask
    , content_url
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.))

import qualified Taskell.Data.Task              as T (Task, new)
import           Taskell.IO.HTTP.Aeson          (deriveFromJSON)
import           Taskell.IO.HTTP.GitHub.Utility (cleanUp)

data MaybeCard = MaybeCard
    { _note        :: Maybe Text
    , _content_url :: Maybe Text
    } deriving (Eq, Show)

-- strip underscores from field labels
$(deriveFromJSON ''MaybeCard)

-- create lenses
$(makeLenses ''MaybeCard)

-- operations
maybeCardToTask :: MaybeCard -> Maybe T.Task
maybeCardToTask card = T.new . cleanUp <$> card ^. note
