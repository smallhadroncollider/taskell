module Taskell.UI.Draw.Modal.TaskList where 

import ClassyPrelude 
import Taskell.UI.Draw.Types (ModalWidget)
import Brick.Widgets.Core (vBox)

taskList :: ModalWidget 
taskList = do 
  let w = vBox []
  pure ("Sub-task List", w)
