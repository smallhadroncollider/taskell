module Data.Taskell.Task where

import Data.Sequence as S (Seq, null, (|>), adjust', empty, length, filter, deleteAt)
import Data.Text as T (Text, snoc, length, null, isInfixOf, empty, append)
import qualified Data.Taskell.Text as T
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)

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

blankSubTask :: SubTask
blankSubTask = SubTask { name = "", complete = False }

subTask :: Text -> Bool -> SubTask
subTask n c = SubTask { name = n, complete = c }

addSubTask :: SubTask -> Task -> Task
addSubTask s t = t { subTasks = subTasks t |> s }

hasSubTasks :: Task -> Bool
hasSubTasks t = not (S.null (subTasks t))

updateSubTask :: Int -> (SubTask -> SubTask) -> Task -> Task
updateSubTask index fn task = task { subTasks = sts }
    where sts = adjust' fn index (subTasks task)

removeSubTask :: Int -> Task -> Task
removeSubTask index task = task { subTasks = S.deleteAt index (subTasks task) }

toggleComplete :: SubTask -> SubTask
toggleComplete st = st { complete = not (complete st) }

append :: Char -> Task  -> Task
append c t = t { description = T.snoc (description t) c }

appendByteString :: ByteString -> Task  -> Task
appendByteString bs t = t { description = T.append (description t) $ decodeUtf8 bs }

backspace :: Task -> Task
backspace t = t { description = T.backspace (description t) }

stAppend :: Char -> SubTask  -> SubTask
stAppend c st = st { name = T.snoc (name st) c }

stBackspace :: SubTask -> SubTask
stBackspace st = st { name = T.backspace (name st) }

stAppendByteString :: ByteString -> SubTask  -> SubTask
stAppendByteString bs st = st { name = T.append (name st) $ decodeUtf8 bs }

countSubTasks :: Task -> Int
countSubTasks = S.length . subTasks

countCompleteSubTasks :: Task -> Int
countCompleteSubTasks = S.length . S.filter complete . subTasks

characters :: Task -> Int
characters = T.length . description

contains :: Text -> Task -> Bool
contains s t = s `T.isInfixOf` description t || not (S.null sts)
    where sts = S.filter (T.isInfixOf s) $ name <$> subTasks t

isBlank :: Task -> Bool
isBlank t = T.null $ description t
