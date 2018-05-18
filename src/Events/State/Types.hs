{-# LANGUAGE NoImplicitPrelude #-}
module Events.State.Types where

import ClassyPrelude

import Data.Taskell.Lists (Lists)
import UI.Field (Field)

data DetailMode = DetailNormal | DetailInsert Field deriving (Eq, Show)
data DetailItem = DetailItem Int | DetailDescription | DetailDate deriving (Eq, Show)

data ModalType = Help | MoveTo | Detail DetailItem DetailMode deriving (Eq, Show)

data InsertType = ITask | IList deriving (Eq, Show)
data InsertMode = IEdit | ICreate deriving (Eq, Show)
data Mode =
    Normal
  | Insert {
    _type :: InsertType,
    _mode :: InsertMode,
    _field :: Field
  }
  | Modal ModalType
  | Search Bool Field
  | Shutdown
  deriving (Eq, Show)

type Pointer = (Int, Int)

data State = State {
    mode :: Mode,
    lists :: Lists,
    history :: [(Pointer, Lists)],
    current :: Pointer,
    path :: FilePath,
    io :: Maybe Lists
} deriving (Eq, Show)

type Stateful = State -> Maybe State
type InternalStateful = State -> State
