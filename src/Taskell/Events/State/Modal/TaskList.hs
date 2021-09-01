module Taskell.Events.State.Modal.TaskList where

import ClassyPrelude
import Control.Lens ((&), (.~), (^.))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Taskell.Data.Lists as L
import Taskell.Events.State.Types (Stateful, lists, mode)
import Taskell.Events.State.Types.Mode (ModalType (TaskList), Mode (Modal))

showTaskList :: Stateful
showTaskList state = setMode (L.subTaskList $ state ^. lists) 0 state
  where
    setMode due pos state = pure $ state & mode .~ Modal (TaskList due pos)
