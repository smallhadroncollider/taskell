{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.State.Types where

import ClassyPrelude

import Control.Lens             (Lens', makeLenses)
import Control.Lens.Combinators (_1, _2)

import Data.Taskell.Lists (Lists)
import Types              (Pointer, startPointer)
import UI.Draw.Field      (Field)

import qualified Events.State.Types.Mode as M (Mode)

type Moment = (Pointer, Lists)

data History a = History
    { _past    :: [a]
    , _present :: a
    , _future  :: [a]
    } deriving (Eq, Show)

fresh :: Lists -> History Moment
fresh ls = History empty (startPointer, ls) empty

data State = State
    { _mode       :: M.Mode
    , _history    :: History Moment
    , _path       :: FilePath
    , _io         :: Maybe Lists
    , _height     :: Int
    , _searchTerm :: Maybe Field
    , _time       :: UTCTime
    } deriving (Eq, Show)

-- create lenses
$(makeLenses ''State)

$(makeLenses ''History)

type Stateful = State -> Maybe State

current :: Lens' State Pointer
current = history . present . _1

lists :: Lens' State Lists
lists = history . present . _2
