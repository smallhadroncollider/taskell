{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Taskell.Events.State.Types where

import ClassyPrelude

import Control.Lens             (Lens', makeLenses)
import Control.Lens.Combinators (_1, _2)

import Data.Time.Zones (TZ)

import Taskell.Data.Lists    (Lists)
import Taskell.Types         (Pointer, startPointer)
import Taskell.UI.Draw.Field (Field)

import qualified Taskell.Events.State.Types.Mode as M (Mode)

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
    , _timeZone   :: TZ
    } deriving (Eq, Show)

-- create lenses
$(makeLenses ''State)

$(makeLenses ''History)

type Stateful = State -> Maybe State

current :: Lens' State Pointer
current = history . present . _1

lists :: Lens' State Lists
lists = history . present . _2
