{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Markdown (
    parse,
    stringify
) where

import ClassyPrelude

import Data.Sequence (adjust')
import Data.Text as T (strip, dropAround, append)
import Data.Text.Encoding (decodeUtf8With)

import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (List, title, tasks, updateFn, count)
import Data.Taskell.Task (Task, SubTask, new, description, subTasks, addSubTask, subTask, name, complete)

import IO.Config (Config, MarkdownConfig, markdown, titleOutput, taskOutput, subtaskOutput)

-- parse code
trim :: Int -> Text -> Text
trim i = strip . drop i

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
    | titleO `isPrefixOf` s = (newList (trim (length titleO) s) ls, errs)
    | taskO `isPrefixOf` s = (appendToLast (new $ trim (length taskO) s) ls, errs)
    | staskO `isPrefixOf` s = (addSubItem (trim (length staskO) s) ls, errs)
    | not (null (strip s)) = (ls, errs ++ [li])
    | otherwise = (ls, errs)
    where titleO = titleOutput config `snoc` ' '
          taskO = taskOutput config `snoc` ' '
          staskO = subtaskOutput config `snoc` ' '

decodeError :: String -> Maybe Word8 -> Maybe Char
decodeError _ _ = Just '\65533'

parse :: Config -> ByteString -> Either String Lists
parse config s = do
    let lns = lines $ decodeUtf8With decodeError s
    let fn = start (markdown config)
    let acc = (empty, [])
    let (lists, errs) = foldl' fn acc $ zip lns [1..]

    if null errs
        then Right lists
        else Left $ "could not parse line(s) " ++ intercalate ", " (show <$> errs)


-- stringify code
subTaskToString :: MarkdownConfig -> Text -> SubTask -> Text
subTaskToString config t st = foldl' append t [
        subtaskOutput config,
        " ",
        surround,
        name st,
        surround,
        "\n"
    ]
    where surround = if complete st then "~" else ""

taskToString :: MarkdownConfig -> Text -> Task -> Text
taskToString config s t = foldl' append s [
        taskOutput config,
        " ",
        description t,
        "\n",
        foldl' (subTaskToString config) "" (subTasks t)
    ]

listToString :: MarkdownConfig -> Text -> List -> Text
listToString config s l = foldl' append s [
        if null s then "" else "\n"
      , titleOutput config
      , " "
      , title l
      , "\n\n"
      , foldl' (taskToString config) "" (tasks l)
    ]

stringify :: Config -> Lists -> ByteString
stringify config ls = encodeUtf8 $ foldl' (listToString (markdown config)) "" ls
