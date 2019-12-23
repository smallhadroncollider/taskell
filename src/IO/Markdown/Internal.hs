{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Markdown.Internal where

import ClassyPrelude

import Control.Lens ((.~), (^.))

import           Data.Sequence      (adjust')
import qualified Data.Text          as T (splitOn, strip)
import           Data.Text.Encoding (decodeUtf8With)

import Data.Time.Zones (TZ)

import           Data.Taskell.Date    (Due, textToTime, timeToOutput, timeToOutputLocal)
import           Data.Taskell.List    (List, count, tasks, title, updateFn)
import           Data.Taskell.Lists   (Lists, appendToLast, newList)
import qualified Data.Taskell.Subtask as ST (Subtask, complete, name, new)
import qualified Data.Taskell.Task    as T (Task, addSubtask, appendDescription, description, due,
                                            name, new, subtasks)

import qualified IO.Config          as C (Config, markdown)
import           IO.Config.Markdown (Config, descriptionOutput, dueOutput, localTimes,
                                     subtaskOutput, taskOutput, titleOutput)

data MarkdownInfo = MarkdownInfo
    { mdTZ     :: TZ
    , mdConfig :: Config
    }

type ReaderMarkdown = Reader MarkdownInfo Text

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
            if not (null (T.strip text))
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
subtaskSymbol :: Bool -> Text
subtaskSymbol True  = "[x]"
subtaskSymbol False = "[ ]"

subtaskStringify :: ST.Subtask -> ReaderMarkdown
subtaskStringify st = do
    symbol <- subtaskOutput <$> asks mdConfig
    pure . concat $ [symbol, " ", subtaskSymbol (st ^. ST.complete), " ", st ^. ST.name]

descriptionStringify :: Text -> ReaderMarkdown
descriptionStringify desc = do
    symbol <- descriptionOutput <$> asks mdConfig
    let add d = concat [symbol, " ", d]
    pure . intercalate "\n" $ add <$> T.splitOn "\n" desc

dueStringify :: Due -> ReaderMarkdown
dueStringify time = do
    symbol <- dueOutput <$> asks mdConfig
    useLocal <- localTimes <$> asks mdConfig
    tz <- asks mdTZ
    let fn =
            if useLocal
                then timeToOutputLocal tz
                else timeToOutput
    pure $ concat [symbol, " ", fn time]

nameStringify :: Text -> ReaderMarkdown
nameStringify desc = do
    symbol <- taskOutput <$> asks mdConfig
    pure $ concat [symbol, " ", desc]

taskStringify :: T.Task -> ReaderMarkdown
taskStringify t = do
    nameString <- nameStringify (t ^. T.name)
    dueString <- fromMaybe "" <$> sequence (dueStringify <$> t ^. T.due)
    descriptionString <- fromMaybe "" <$> sequence (descriptionStringify <$> t ^. T.description)
    subtaskString <- intercalate "\n" <$> sequence (subtaskStringify <$> t ^. T.subtasks)
    pure . unlines . filter (/= "") $ [nameString, dueString, descriptionString, subtaskString]

listStringify :: List -> ReaderMarkdown
listStringify list = do
    symbol <- titleOutput <$> asks mdConfig
    taskString <- concat <$> sequence (taskStringify <$> list ^. tasks)
    pure $ concat [symbol, " ", list ^. title, "\n\n", taskString]

stringify :: Lists -> ReaderMarkdown
stringify ls = intercalate "\n" <$> sequence (listStringify <$> ls)
