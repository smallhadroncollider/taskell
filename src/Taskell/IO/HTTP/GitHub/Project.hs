{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.HTTP.GitHub.Project
    ( Project
    , columnsURL
    , name
    ) where

import ClassyPrelude

import Control.Lens (Lens', makeLenses)

import Taskell.IO.HTTP.Aeson (deriveFromJSON)

data Project = Project
    { _name        :: Text
    , _columns_url :: Text
    } deriving (Eq, Show)

-- create Aeson code
$(deriveFromJSON ''Project)

-- create lenses
$(makeLenses ''Project)

-- operations
columnsURL :: Lens' Project Text
columnsURL = columns_url
