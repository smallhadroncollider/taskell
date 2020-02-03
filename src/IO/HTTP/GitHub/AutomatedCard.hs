{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IO.HTTP.GitHub.AutomatedCard
    ( AutomatedCard(AutomatedCard)
    , automatedCardToTask
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.))
import Data.Text    (replace)

import qualified Data.Taskell.Task as T (Task, new, setDescription)
import           IO.HTTP.Aeson     (deriveFromJSON)

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
automatedCardToTask automatedCard = T.setDescription (automatedCard ^. body) task
  where
    task = T.new $ replace "\r" "" $ replace "\n" " " (automatedCard ^. title)
