module Taskell.Events.State.Modal.TaskList where 

import ClassyPrelude 

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Taskell.Events.State.Types (Stateful, mode)
import Control.Lens ((&), (.~))
import Taskell.Events.State.Types.Mode (ModalType(TaskList), Mode (Modal))

showTaskList :: Stateful 
showTaskList state = setMode Seq.empty 0 state
  where 
    setMode due pos state = pure $ state & mode .~ Modal (TaskList due pos)


