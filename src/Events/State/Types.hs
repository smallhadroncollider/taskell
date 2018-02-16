module Events.State.Types where

import Data.Text (Text)
import Data.Taskell.Lists (Lists)

data SubTasksMode = STNormal | STInsert
data ModalType = Help | SubTasks { stIndex :: Int, stMode :: SubTasksMode }

data InsertMode = EditTask | CreateTask | EditList | CreateList Text
data Mode = Normal | Insert InsertMode | Write Mode | Modal ModalType | Search Bool Text | Shutdown

type Pointer = (Int, Int)

data State = State {
    mode :: Mode,
    lists :: Lists,
    history :: [(Pointer, Lists)],
    current :: Pointer,
    path :: FilePath
}

type Stateful = State -> Maybe State
type InternalStateful = State -> State
