{-# LANGUAGE NoImplicitPrelude #-}

module Events.State.Types.Mode where

import ClassyPrelude

import Data.Taskell.Task (Task)
import Types             (Pointer)
import UI.Draw.Field     (Field)

data DetailMode
    = DetailNormal
    | DetailInsert Field
    deriving (Eq, Show)

data DetailItem
    = DetailItem Int
    | DetailDescription
    | DetailDate
    deriving (Eq, Show)

data ModalType
    = Help
    | MoveTo
    | Due (Seq (Pointer, Task))
          Int
    | Detail DetailItem
             DetailMode
    deriving (Eq, Show)

data InsertType
    = ITask
    | IList
    deriving (Eq, Show)

data InsertMode
    = IEdit
    | ICreate
    deriving (Eq, Show)

data Mode
    = Normal
    | Insert InsertType
             InsertMode
             Field
    | Modal ModalType
    | Search
    | Shutdown
    deriving (Eq, Show)
