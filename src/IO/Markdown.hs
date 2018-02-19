{-# LANGUAGE OverloadedStrings #-}
module IO.Markdown (
    parse,
    stringify,
    trimListItem
) where

import Data.Text as T (Text, drop, append, null, lines, isPrefixOf, strip, dropAround, snoc)
import Data.List (intercalate)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)

import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (List, title, tasks, updateFn, count)
import Data.Taskell.Task (Task, SubTask, new, description, subTasks, addSubTask, subTask, name, complete)
import Data.Foldable (foldl')
import Data.Sequence (empty, adjust')
import Data.ByteString (ByteString)
import Data.Word (Word8)

import IO.Config (Config, MarkdownConfig, markdown, titleOutput, taskOutput, subtaskOutput)

-- parse code
trimListItem :: Text -> Text
trimListItem = strip . T.drop 1

trimTitle :: Text -> Text
trimTitle = strip . T.drop 2

trimTask :: Text -> Task
trimTask = new . trimListItem

trimTilde :: Text -> Text
trimTilde = strip . T.dropAround (== '~')

addSubItem :: Text -> Lists -> Lists
addSubItem t ls = adjust' updateList i ls
    where i = length ls - 1
          st | "~" `isPrefixOf` t = subTask (trimTilde t) True
             | otherwise = subTask t False
          updateList l = updateFn j (addSubTask st) l
            where j = count l - 1

start :: MarkdownConfig -> (Lists, [Int]) -> (Text, Int) -> (Lists, [Int])
start config (ls, errs) (s, li)
    | titleOutput config `snoc` ' ' `isPrefixOf` s = (newList (trimTitle s) ls, errs)
    | taskOutput config `snoc` ' ' `isPrefixOf` s = (appendToLast (trimTask s) ls, errs)
    | subtaskOutput config `snoc` ' ' `isPrefixOf` s = (addSubItem (trimListItem $ strip s) ls, errs)
    | not (T.null (strip s)) = (ls, errs ++ [li])
    | otherwise = (ls, errs)

decodeError :: String -> Maybe Word8 -> Maybe Char
decodeError _ _ = Just '\65533'

parse :: Config -> ByteString -> Either String Lists
parse config s = do
    let lns = T.lines $ decodeUtf8With decodeError s
    let fn = start (markdown config)
    let acc = (empty, [])
    let (lists, errs) = foldl' fn acc $ zip lns [1..]

    if Prelude.null errs
        then Right lists
        else Left $ "could not parse line(s) " ++ intercalate ", " (show <$> errs)


-- stringify code
join :: Text -> [Text] -> Text
join = foldl' T.append

subTaskToString :: MarkdownConfig -> Text -> SubTask -> Text
subTaskToString config t st = join t [
        subtaskOutput config,
        " ",
        surround,
        name st,
        surround,
        "\n"
    ]
    where surround = if complete st then "~" else ""

taskToString :: MarkdownConfig -> Text -> Task -> Text
taskToString config s t = join s [
        taskOutput config,
        " ",
        description t,
        "\n",
        foldl' (subTaskToString config) "" (subTasks t)
    ]

listToString :: MarkdownConfig -> Text -> List -> Text
listToString config s l = join s [
        if T.null s then "" else "\n"
      , titleOutput config
      , " "
      , title l
      , "\n\n"
      , foldl' (taskToString config) "" (tasks l)
    ]

stringify :: Config -> Lists -> ByteString
stringify config ls = encodeUtf8 $ foldl' (listToString (markdown config)) "" ls
