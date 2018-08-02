{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Markdown.Internal where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Sequence (adjust')
import Data.Text as T (strip, dropAround, splitOn)
import Data.Text.Encoding (decodeUtf8With)

import Data.Taskell.Date (dayToOutput)
import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (List, title, tasks, updateFn, count)
import qualified Data.Taskell.Subtask as ST (Subtask, new, name, complete)
import qualified Data.Taskell.Task as T (Task, new, name, subtasks, addSubtask, appendDescription, setDue, due, description)

import qualified IO.Config as C (Config, markdown)
import IO.Config.Markdown (Config, titleOutput, taskOutput, descriptionOutput, dueOutput, subtaskOutput)

-- parse code
trimTilde :: Text -> Text
trimTilde = strip . T.dropAround (== '~')

addSubItem :: Text -> Lists -> Lists
addSubItem t ls = adjust' updateList i ls
    where i = length ls - 1
          st | "[ ] " `isPrefixOf` t = ST.new (drop 4 t) False
             | "[x] " `isPrefixOf` t = ST.new (drop 4 t) True
             | "~" `isPrefixOf` t = ST.new (trimTilde t) True
             | otherwise = ST.new t False
          updateList l = updateFn j (T.addSubtask st) l
            where j = count l - 1

addDescription :: Text -> Lists -> Lists
addDescription t ls = adjust' updateList i ls
    where i = length ls - 1
          updateList l = updateFn j (T.appendDescription t) l
            where j = count l - 1

addDue :: Text -> Lists -> Lists
addDue t ls = adjust' updateList i ls
    where i = length ls - 1
          updateList l = updateFn j (T.setDue t) l
            where j = count l - 1

prefix :: Config -> Text -> (Config -> Text) -> (Text -> Lists -> Lists) -> Maybe (Lists -> Lists)
prefix config str get set
    | pre `isPrefixOf` str = Just $ set (drop (length pre) str)
    | otherwise = Nothing
    where pre = get config `snoc` ' '

matches :: [(Config -> Text, Text -> Lists -> Lists)]
matches = [
        (titleOutput, newList)
      , (taskOutput, appendToLast . T.new)
      , (descriptionOutput, addDescription)
      , (dueOutput, addDue)
      , (subtaskOutput, addSubItem)
    ]

start :: Config -> (Lists, [Int]) -> (Text, Int) -> (Lists, [Int])
start config (current, errs) (text, line) =
    case find isJust $ uncurry (prefix config text) <$> matches of
        Just (Just set) -> (set current, errs)
        _ -> if not (null (strip text))
            then (current, errs ++ [line])
            else (current, errs)


decodeError :: String -> Maybe Word8 -> Maybe Char
decodeError _ _ = Just '\65533'

parse :: C.Config -> ByteString -> Either Text Lists
parse config s = do
    let lns = lines $ decodeUtf8With decodeError s
    let fn = start (C.markdown config)
    let acc = (empty, [])
    let (lists, errs) = foldl' fn acc $ zip lns [1..]

    if null errs
        then Right lists
        else Left $ "could not parse line(s) " ++ intercalate ", " (tshow <$> errs)


-- stringify code
subtaskStringify :: Config -> Text -> ST.Subtask -> Text
subtaskStringify config t st = foldl' (++) t [
        subtaskOutput config,
        " ",
        pre,
        " ",
        st ^. ST.name,
        "\n"
    ]
    where pre = if st ^. ST.complete then "[x]" else "[ ]"

descriptionStringify :: Config -> Text -> Text
descriptionStringify config desc = concat $ add <$> splitOn "\n" desc
    where add d = concat [descriptionOutput config, " ", d, "\n"]

dueStringify :: Config -> Day -> Text
dueStringify config day = concat [dueOutput config, " ", dayToOutput day, "\n"]

nameStringify :: Config -> Text -> Text
nameStringify config desc = concat [taskOutput config, " ", desc, "\n"]

taskStringify :: Config -> Text -> T.Task -> Text
taskStringify config s t = foldl' (++) s [
        nameStringify config (t ^. T.name),
        maybe "" (dueStringify config) (t ^. T.due),
        maybe "" (descriptionStringify config) (t ^. T.description),
        foldl' (subtaskStringify config) "" (t ^. T.subtasks)
    ]

listStringify :: Config -> Text -> List -> Text
listStringify config text list = foldl' (++) text [
        if null text then "" else "\n"
      , titleOutput config
      , " "
      , list ^. title
      , "\n\n"
      , foldl' (taskStringify config) "" (list ^. tasks)
    ]

stringify :: C.Config -> Lists -> ByteString
stringify config ls = encodeUtf8 $ foldl' (listStringify (C.markdown config)) "" ls
