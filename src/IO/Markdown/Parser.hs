{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Markdown.Parser where

import ClassyPrelude

import Control.Lens ((.~))

import           Data.Sequence      (adjust')
import qualified Data.Text          as T (strip)
import           Data.Text.Encoding (decodeUtf8With)

import Data.Time.Zones (TZ)

import           Data.Taskell.Date    (textToTime)
import           Data.Taskell.List    (count, updateFn)
import           Data.Taskell.Lists   (Lists, appendToLast, newList)
import qualified Data.Taskell.Subtask as ST (new)
import qualified Data.Taskell.Task    as T (addSubtask, appendDescription, due, new)

import qualified IO.Config          as C (Config, markdown)
import           IO.Config.Markdown (Config, descriptionOutput, dueOutput, subtaskOutput,
                                     taskOutput, titleOutput)

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
