{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.IO.Markdown.Parser
    ( parse
    ) where

import ClassyPrelude

import Data.Attoparsec.Text hiding (parse)

import qualified Taskell.Data.Date    as D (Due, textToTime)
import qualified Taskell.Data.List    as L (List, create)
import qualified Taskell.Data.Lists   as LS (Lists)
import qualified Taskell.Data.Subtask as ST (Subtask, new)
import qualified Taskell.Data.Task    as T (Task, create)

import Taskell.IO.Config.Markdown (Config, descriptionOutput, dueOutput, subtaskOutput, taskOutput,
                                   titleOutput)
import Taskell.Utility.Parser     (lexeme, line)

-- config symbol parsing
type Symbol = (Config -> Text) -> Parser ()

symP :: Config -> Symbol
symP config fn = string (fn config) *> char ' ' $> ()

-- utility functions
emptyMay :: (MonoFoldable a) => a -> Maybe a
emptyMay a =
    if null a
        then Nothing
        else Just a

-- parsers
subtaskCompleteP :: Parser Bool
subtaskCompleteP = (== 'x') <$> (char '[' *> (char 'x' <|> char ' ') <* char ']' <* char ' ')

subtaskP :: Symbol -> Parser ST.Subtask
subtaskP sym = flip ST.new <$> (sym subtaskOutput *> subtaskCompleteP) <*> line

taskDescriptionP :: Symbol -> Parser (Maybe Text)
taskDescriptionP sym = emptyMay <$> (intercalate "\n" <$> many' (sym descriptionOutput *> line))

dueP :: Symbol -> Parser (Maybe D.Due)
dueP sym = (D.textToTime =<<) <$> optional (sym dueOutput *> line)

taskNameP :: Symbol -> Parser Text
taskNameP sym = sym taskOutput *> line

taskP :: Symbol -> Parser T.Task
taskP sym =
    T.create <$> taskNameP sym <*> dueP sym <*> taskDescriptionP sym <*>
    (fromList <$> many' (subtaskP sym))

listTitleP :: Symbol -> Parser Text
listTitleP sym = lexeme $ sym titleOutput *> line

listP :: Symbol -> Parser L.List
listP sym = L.create <$> listTitleP sym <*> (fromList <$> many' (taskP sym))

markdownP :: Symbol -> Parser LS.Lists
markdownP sym = fromList <$> many1 (listP sym) <* endOfInput

-- parse
parse :: Config -> Text -> Either Text LS.Lists
parse config txt = first (const "Could not parse file.") (parseOnly (markdownP (symP config)) txt)
