{-# LANGUAGE OverloadedStrings #-}
module IO.Markdown (
    parse,
    stringify,
    trimListItem
) where

import Data.Text (Text, drop, append, null, lines, isPrefixOf, strip, dropAround)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)

import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (List, title, tasks, updateFn, count)
import Data.Taskell.Task (Task, SubTask, new, description, subTasks, addSubTask, subTask, name, complete)
import Data.Foldable (foldl')
import Data.Sequence (empty, adjust')
import Data.ByteString (ByteString)
import Data.Word (Word8)

-- parse code
trimListItem :: Text -> Text
trimListItem = strip . Data.Text.drop 1

trimTitle :: Text -> Text
trimTitle = strip . Data.Text.drop 2

trimTask :: Text -> Task
trimTask = new . trimListItem

trimTilde :: Text -> Text
trimTilde = strip . Data.Text.dropAround (== '~')

addSubItem :: Text -> Lists -> Lists
addSubItem t ls = adjust' updateList i ls
    where i = length ls - 1
          st | "~" `isPrefixOf` t = subTask (trimTilde t) True
             | otherwise = subTask t False
          updateList l = updateFn j (addSubTask st) l
            where j = count l - 1

start :: Lists -> Text -> Lists
start ls s | "##" `isPrefixOf` s = newList (trimTitle s) ls
           | "-" `isPrefixOf` s = appendToLast (trimTask s) ls
           | "    *" `isPrefixOf` s = addSubItem (trimListItem $ strip s) ls
           | otherwise = ls

decodeError :: String -> Maybe Word8 -> Maybe Char
decodeError _ _ = Just '\65533'

parse :: ByteString -> Lists
parse s = foldl' start empty $ Data.Text.lines $ decodeUtf8With decodeError s

-- stringify code
join :: Text -> [Text] -> Text
join = foldl' Data.Text.append

subTaskToString :: Text -> SubTask -> Text
subTaskToString t st = join t ["    * ", surround, name st, surround, "\n"]
    where surround = if complete st then "~" else ""

taskToString :: Text -> Task -> Text
taskToString s t = join s ["- ", description t, "\n", foldl' subTaskToString "" (subTasks t)]

listToString :: Text -> List -> Text
listToString s l = join s [
        if Data.Text.null s then "" else "\n"
      , "## "
      , title l
      , "\n\n"
      , foldl' taskToString "" (tasks l)
    ]

stringify :: Lists -> ByteString
stringify ls = encodeUtf8 $ foldl' listToString "" ls
