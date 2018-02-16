module Data.Taskell.Task where

import Data.Sequence as S (Seq, null, (|>), adjust', empty)
import Data.Text as T (Text, snoc, length, null, isInfixOf, empty)
import qualified Data.Taskell.Text as T

data SubTask = SubTask {
    name :: Text,
    complete :: Bool
} deriving (Show, Eq)

data Task = Task {
    description :: Text,
    subTasks :: Seq SubTask
} deriving (Show, Eq)

blank :: Task
blank = Task { description = T.empty, subTasks = S.empty }

clear :: Task -> Task
clear _ = blank

new :: Text -> Task
new s = blank { description = s }

subTask :: Text -> Bool -> SubTask
subTask n c = SubTask { name = n, complete = c }

addSubTask :: SubTask -> Task -> Task
addSubTask s t = t { subTasks = subTasks t |> s }

hasSubTasks :: Task -> Bool
hasSubTasks t = not (S.null (subTasks t))

updateSubTask :: Int -> (SubTask -> SubTask) -> Task -> Task
updateSubTask index fn task = task { subTasks = sts }
    where sts = adjust' fn index (subTasks task)

toggleComplete :: SubTask -> SubTask
toggleComplete st = st { complete = not (complete st) }

append :: Char -> Task  -> Task
append c t = t { description = T.snoc (description t) c }

backspace :: Task -> Task
backspace t = t { description = T.backspace (description t) }

characters :: Task -> Int
characters = T.length . description

contains :: Text -> Task -> Bool
contains s t = s `T.isInfixOf` description t

isBlank :: Task -> Bool
isBlank t = T.null $ description t
