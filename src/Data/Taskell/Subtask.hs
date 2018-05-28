{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Subtask where

import ClassyPrelude
import Control.Lens (makeLenses, (%~))

data Subtask = Subtask {
    _name :: Text,
    _complete :: Bool
} deriving (Show, Eq)

-- create lenses
$(makeLenses ''Subtask)


-- operations
blank :: Subtask
blank = Subtask "" False

new :: Text -> Bool -> Subtask
new = Subtask

toggle :: Subtask -> Subtask
toggle = complete %~ not
