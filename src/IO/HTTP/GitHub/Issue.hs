{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IO.HTTP.GitHub.Issue
    ( Issue(Issue)
    , issueToTask
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (^.))
import Data.Text    (replace)

import qualified Data.Taskell.Task as T (Task, new, setDescription)
import           IO.HTTP.Aeson     (deriveFromJSON)

data Issue = Issue
    { _title :: Text
    , _body  :: Text
    } deriving (Eq, Show)

-- strip underscores from field labels
$(deriveFromJSON ''Issue)

-- create lenses
$(makeLenses ''Issue)

-- operations
issueToTask :: Issue -> T.Task
issueToTask issue = T.setDescription (issue ^. body) task
  where
    task = T.new $ replace "\r" "" $ replace "\n" " " (issue ^. title)
