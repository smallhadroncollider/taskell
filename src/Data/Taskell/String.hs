{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.String (
    backspace,
    trunc,
    wrap,
    trim
) where

import Prelude hiding (words, unwords, length, take)
import Data.Text (Text, foldl', singleton, snoc, unwords, words, length, take, init, null, append, splitOn)
import Data.List (foldl')

backspace :: Text -> Text
backspace s = if not (Data.Text.null s) then Data.Text.init s else s

trunc :: Int -> Text -> Text
trunc width s = if length s > width then take (width - 3) s `Data.Text.append` "..." else s

trim :: Text -> Text
trim = unwords . words

-- wrapping
spl' :: [Text] -> Char -> [Text]
spl' ts c
    | c == ' ' = ts ++ [" "] ++ []
    | Prelude.null ts = [Data.Text.singleton c]
    | otherwise = Prelude.init ts ++ [Data.Text.snoc (Prelude.last ts) c]

spl :: Text -> [Text]
spl = Data.Text.foldl' spl' []

wrap :: Int -> Text -> [Text]
wrap width = Data.List.foldl' (combine width) [] . spl

combine :: Int -> [Text] -> Text -> [Text]
combine width acc s = if nl then acc ++ [trim s] else Data.Taskell.String.append (l `Data.Text.append` s) acc
    where l = if Prelude.null acc then "" else last acc
          nl = length l + length s > width

append :: Text -> [Text] -> [Text]
append s l = l' ++ [s]
    where l' = if Prelude.null l then l else Prelude.init l
