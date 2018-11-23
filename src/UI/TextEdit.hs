{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.TextEdit (
    textEdit
  , getText
  , getCursor
  , getWrapped
) where

import ClassyPrelude hiding (lines)

import Data.Text (splitOn)

type Cursor = (Int, Int)
type Lines = [Text]

data TextEdit = TextEdit Lines Cursor

textEdit :: Text -> TextEdit
textEdit text = TextEdit lines (x, y)
    where lines = splitOn "\n" text
          y = length lines - 1
          x = maybe 0 length (lastMay lines)

getText :: TextEdit -> Text
getText (TextEdit lines _) = intercalate "\n" lines

getCursor :: TextEdit -> Cursor
getCursor (TextEdit _ cursor) = cursor

getWrapped :: Int -> TextEdit -> ([Text], Cursor)
getWrapped _ (TextEdit lines cursor) = (split =<< lines, cursor)

appendToLast :: Char -> [Text] -> [Text]
appendToLast char parts = case fromNullable parts of
    Just parts' -> init parts' ++ [snoc (last parts') char]
    Nothing -> [singleton char]

pullSpaces :: [Text] -> Char -> [Text]
pullSpaces parts char
    | char == ' ' = parts ++ [" "] ++ [""]
    | otherwise = appendToLast char parts

split :: Text -> [Text]
split = foldl' pullSpaces []
