module UI.Task where

import Config (width)
import Data.Taskell.Task (Task, description)
import Data.Taskell.String (wrap)

type CursorPosition = (Int, Int)
data TaskUI = TaskUI [String] CursorPosition 

present :: Task -> TaskUI
present t = TaskUI wrapped cp
    where wrapped = wrap width $ description t
          cp = (length wrapped, length (last wrapped))
