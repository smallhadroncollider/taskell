{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IO.HTTP.GitHub.AutomatedCard
    ( AutomatedCard(AutomatedCard)
    , automatedCardToTask
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.))

import qualified Data.Taskell.Task      as T (Task, new, setDescription)
import           IO.HTTP.Aeson          (deriveFromJSON)
import           IO.HTTP.GitHub.Utility (cleanUp)

data AutomatedCard = AutomatedCard
    { _title :: Text
    , _body  :: Text
    } deriving (Eq, Show)

-- strip underscores from field labels
$(deriveFromJSON ''AutomatedCard)

-- create lenses
$(makeLenses ''AutomatedCard)

-- operations
automatedCardToTask :: AutomatedCard -> T.Task
automatedCardToTask automatedCard = T.setDescription (cleanUp (automatedCard ^. body)) task
  where
    task = T.new $ cleanUp (automatedCard ^. title)
