{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Subtask.Internal where

import ClassyPrelude
import Control.Lens  (makeLenses, (%~))

data Subtask = Subtask
    { _name     :: Text
    , _complete :: Bool
    } deriving (Show, Eq)

type Update = Subtask -> Subtask

-- create lenses
$(makeLenses ''Subtask)

-- operations
blank :: Subtask
blank = Subtask "" False

new :: Text -> Bool -> Subtask
new = Subtask

toggle :: Update
toggle = complete %~ not

duplicate :: Subtask -> Subtask
duplicate (Subtask n c) = Subtask n c
