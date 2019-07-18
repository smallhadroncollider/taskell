{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.State.Types where

import ClassyPrelude

import Control.Lens (makeLenses)

import Data.Time          (UTCTime)
import Data.Taskell.Lists (Lists)

import qualified Events.State.Types.Mode as M (Mode)

type Pointer = (Int, Int)

--------------------------------------------------------------------------------

data PomodoroPhase
    = Task
    | ShortBreak
    | LongBreak
    deriving (Show, Eq)

data Pomodoro = Pomodoro
    { _phase :: PomodoroPhase
    , _task :: Pointer
    , _startTime :: UTCTime
    , _cycle :: Int -- 4 cycles equals long break instead of short, plus a
                    -- cycle reset on next task
    }
    deriving (Show, Eq)

--------------------------------------------------------------------------------

data State = State
    { _mode     :: M.Mode
    , _lists    :: Lists
    , _history  :: [(Pointer, Lists)]
    , _current  :: Pointer
    , _path     :: FilePath
    , _io       :: Maybe Lists
    , _pomodoro :: Maybe Pomodoro
    , _time     :: UTCTime
    } deriving (Eq, Show)

-- create lenses
$(makeLenses ''State)

type Stateful = State -> Maybe State
