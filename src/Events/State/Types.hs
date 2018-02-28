module Events.State.Types where

import Data.Taskell.Lists (Lists)
import UI.Field (Field)

data SubTasksMode = STNormal | STInsert Field
data ModalType = Help | MoveTo | SubTasks Int SubTasksMode

data InsertType = ITask | IList
data InsertMode = IEdit | ICreate
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

type Pointer = (Int, Int)

data State = State {
    mode :: Mode,
    lists :: Lists,
    history :: [(Pointer, Lists)],
    current :: Pointer,
    path :: FilePath,
    io :: Maybe Lists
}

type Stateful = State -> Maybe State
type InternalStateful = State -> State
