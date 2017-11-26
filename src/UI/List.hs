module UI.List where

import Data.Sequence (Seq)
import Data.Taskell.List (List(List))
import qualified UI.Task as TaskUI (TaskUI, present)

data ListUI = ListUI String (Seq TaskUI.TaskUI)

present :: List -> ListUI
present (List title ts) = ListUI title tasks
    where tasks = TaskUI.present <$> ts
