module Taskell.Data.Date.Types
    ( Deadline(..)
    , Due(..)
    ) where

import ClassyPrelude

data Due
    = DueTime UTCTime
    | DueDate Day
    deriving (Show, Eq)

instance Ord Due where
    compare (DueTime t) (DueDate d)   = t `compare` UTCTime d 0
    compare (DueDate d) (DueTime t)   = UTCTime d 0 `compare` t
    compare (DueDate d1) (DueDate d2) = d1 `compare` d2
    compare (DueTime t1) (DueTime t2) = t1 `compare` t2

data Deadline
    = Passed
    | Today
    | Tomorrow
    | ThisWeek
    | Plenty
    deriving (Show, Eq)
