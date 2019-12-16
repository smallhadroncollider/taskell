{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Markdown.Internal where

import ClassyPrelude

import Control.Lens ((.~), (^.))

import Data.Sequence      (adjust')
import Data.Text          as T (splitOn, strip)
import Data.Text.Encoding (decodeUtf8With)

import Data.Time.Zones (TZ)

import           Data.Taskell.Date    (Due, textToTime, timeToOutput)
import           Data.Taskell.List    (List, count, tasks, title, updateFn)
import           Data.Taskell.Lists   (Lists, appendToLast, newList)
import qualified Data.Taskell.Subtask as ST (Subtask, complete, name, new)
import qualified Data.Taskell.Task    as T (Task, addSubtask, appendDescription, description, due,
                                            name, new, subtasks)

import qualified IO.Config          as C (Config, markdown)
import           IO.Config.Markdown (Config, descriptionOutput, dueOutput, subtaskOutput,
                                     taskOutput, titleOutput)

-- parse code
addSubItem :: Text -> Lists -> Lists
addSubItem t ls = adjust' updateList i ls
  where
    i = length ls - 1
    st
        | "[ ] " `isPrefixOf` t = ST.new (drop 4 t) False
        | "[x] " `isPrefixOf` t = ST.new (drop 4 t) True
        | otherwise = ST.new t False
    updateList l = updateFn j (T.addSubtask st) l
      where
        j = count l - 1

addDescription :: Text -> Lists -> Lists
addDescription t ls = adjust' updateList i ls
  where
    i = length ls - 1
    updateList l = updateFn j (T.appendDescription t) l
      where
        j = count l - 1

addDue :: Text -> Lists -> Lists
addDue t ls = adjust' updateList i ls
  where
    i = length ls - 1
    updateList l = updateFn j (T.due .~ textToTime t) l
      where
        j = count l - 1

prefix :: Config -> Text -> (Config -> Text) -> (Text -> Lists -> Lists) -> Maybe (Lists -> Lists)
prefix config str get set
    | pre `isPrefixOf` str = Just $ set (drop (length pre) str)
    | otherwise = Nothing
  where
    pre = get config `snoc` ' '

matches :: [(Config -> Text, Text -> Lists -> Lists)]
matches =
    [ (titleOutput, newList)
    , (taskOutput, appendToLast . T.new)
    , (descriptionOutput, addDescription)
    , (dueOutput, addDue)
    , (subtaskOutput, addSubItem)
    ]

start :: Config -> (Lists, [Int]) -> (Text, Int) -> (Lists, [Int])
start config (current, errs) (text, line) =
    case find isJust $ uncurry (prefix config text) <$> matches of
        Just (Just set) -> (set current, errs)
        _ ->
            if not (null (strip text))
                then (current, errs <> [line])
                else (current, errs)

decodeError :: String -> Maybe Word8 -> Maybe Char
decodeError _ _ = Just '\65533'

parse :: C.Config -> ByteString -> Either Text Lists
parse config s = do
    let lns = lines $ decodeUtf8With decodeError s
    let fn = start (C.markdown config)
    let acc = (empty, [])
    let (lists, errs) = foldl' fn acc $ zip lns [1 ..]
    if null errs
        then Right lists
        else Left $ "could not parse line(s) " <> intercalate ", " (tshow <$> errs)

-- stringify code
subtaskStringify :: Config -> Text -> ST.Subtask -> Text
subtaskStringify config t st =
    foldl' (<>) t [subtaskOutput config, " ", pre, " ", st ^. ST.name, "\n"]
  where
    pre =
        if st ^. ST.complete
            then "[x]"
            else "[ ]"

descriptionStringify :: Config -> Text -> Text
descriptionStringify config desc = concat $ add <$> splitOn "\n" desc
  where
    add d = concat [descriptionOutput config, " ", d, "\n"]

dueStringify :: TZ -> Config -> Due -> Text
dueStringify tz config time = concat [dueOutput config, " ", timeToOutput tz time, "\n"]

nameStringify :: Config -> Text -> Text
nameStringify config desc = concat [taskOutput config, " ", desc, "\n"]

taskStringify :: TZ -> Config -> Text -> T.Task -> Text
taskStringify tz config s t =
    foldl'
        (<>)
        s
        [ nameStringify config (t ^. T.name)
        , maybe "" (dueStringify tz config) (t ^. T.due)
        , maybe "" (descriptionStringify config) (t ^. T.description)
        , foldl' (subtaskStringify config) "" (t ^. T.subtasks)
        ]

listStringify :: TZ -> Config -> Text -> List -> Text
listStringify tz config text list =
    foldl'
        (<>)
        text
        [ if null text
              then ""
              else "\n"
        , titleOutput config
        , " "
        , list ^. title
        , "\n\n"
        , foldl' (taskStringify tz config) "" (list ^. tasks)
        ]

stringify :: TZ -> C.Config -> Lists -> ByteString
stringify tz config ls = encodeUtf8 $ foldl' (listStringify tz (C.markdown config)) "" ls
