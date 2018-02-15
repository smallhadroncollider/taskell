module Data.Taskell.Task where

import Data.Text (Text, snoc, length, null, isInfixOf, empty)
import qualified Data.Taskell.Text as T

data SubTask = SubTask {
    name :: Text,
    complete :: Bool
} deriving (Show, Eq)

data Task = Task {
    description :: Text,
    subTasks :: [SubTask]
} deriving (Show, Eq)

blank :: Task
blank = Task { description = empty, subTasks = [] }

clear :: Task -> Task
clear _ = blank

new :: Text -> Task
new s = blank { description = s }

subTask :: Text -> Bool -> SubTask
subTask n c = SubTask { name = n, complete = c }

addSubTask :: SubTask -> Task -> Task
addSubTask s t = t { subTasks = subTasks t ++ [s] }

append :: Char -> Task  -> Task
append c t = t { description = Data.Text.snoc (description t) c }

backspace :: Task -> Task
backspace t = t { description = T.backspace (description t) }

characters :: Task -> Int
characters = Data.Text.length . description

contains :: Text -> Task -> Bool
contains s t = s `Data.Text.isInfixOf` description t

isBlank :: Task -> Bool
isBlank t = Data.Text.null $ description t
