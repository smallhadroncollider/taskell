module Taskell.UI.Draw.Modal.TaskList where

import Brick (Padding (Pad))
import Brick.Widgets.Core (cached, padBottom, padLeft, txt, vBox, visible, withAttr)
import ClassyPrelude
import Control.Lens ((^.))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Taskell.Data.Seq ((<#>))
import Taskell.Data.Subtask (Subtask (Subtask), name)
import qualified Taskell.Data.Task as T
import Taskell.Types (Pointer)
import Taskell.UI.Draw.Task (TaskWidget (TaskWidget), parts)
import Taskell.UI.Draw.Types (DSWidget, ModalWidget, TWidget)
import Taskell.UI.Theme (taskAttr, taskCurrentAttr)
import Taskell.UI.Types (ResourceName (RNTaskList))

taskList :: Seq (Pointer, T.Task) -> Int -> ModalWidget
taskList tasks selectedIndex = do
  let items = snd <$> tasks
      withSubtasks = tasksWithSubtasks items
  widgets <- sequence $ renderTask selectedIndex <#> withSubtasks
  pure
    ( "Sub-task List",
      if null items
        then txt "No Tasks with SubTasks"
        else vBox $ toList widgets
    )

renderTask :: Int -> Int -> T.Task -> DSWidget
renderTask current position task = do
  (TaskWidget text date _ subTaskCount) <- parts task
  let selected = current == position
      subtasks = task ^. T.subtasks
  let attr =
        if selected
          then taskCurrentAttr
          else taskAttr
  let shw =
        if selected
          then visible
          else id
  pure . shw . cached (RNTaskList position)
    . padBottom (Pad 1)
    . withAttr attr
    $ vBox [text, renderSubTasks subtasks]

renderSubTasks :: Seq Subtask -> TWidget
renderSubTasks subtasks = padLeft (Pad 1) . vBox $ renderSubTask <$> toList subtasks

renderSubTask :: Subtask -> TWidget
renderSubTask s = txt $ s ^. name

tasksWithSubtasks :: Seq T.Task -> Seq T.Task
tasksWithSubtasks = Seq.filter hasSubtask
  where
    hasSubtask :: T.Task -> Bool
    hasSubtask t = Seq.length (t ^. T.subtasks) > 0
