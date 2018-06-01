{-# LANGUAGE NoImplicitPrelude #-}
module Events.State.Types where

import ClassyPrelude

import Data.Taskell.Lists (Lists)

import qualified Events.State.Types.Mode as M (Mode)

type Pointer = (Int, Int)

data State = State {
    mode :: M.Mode,
    lists :: Lists,
    history :: [(Pointer, Lists)],
    current :: Pointer,
    path :: FilePath,
    io :: Maybe Lists
} deriving (Eq, Show)

type Stateful = State -> Maybe State
type InternalStateful = State -> State
