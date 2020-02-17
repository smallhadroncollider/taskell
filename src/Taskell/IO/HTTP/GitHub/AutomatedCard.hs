{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.HTTP.GitHub.AutomatedCard
    ( AutomatedCard(AutomatedCard)
    , automatedCardToTask
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.))

import qualified Taskell.Data.Task              as T (Task, new, setDescription)
import           Taskell.IO.HTTP.Aeson          (deriveFromJSON)
import           Taskell.IO.HTTP.GitHub.Utility (cleanUp)

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
