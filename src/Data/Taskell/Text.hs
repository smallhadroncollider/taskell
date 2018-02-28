{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Text (
    wrap
) where

import Data.Text as T (Text, foldl', singleton, snoc, append, strip, empty)
import Data.List (foldl')
import Brick.Widgets.Core (textWidth)

spl' :: [Text] -> Char -> [Text]
spl' ts c
    | c == ' ' = ts ++ [" "] ++ [empty]
    | Prelude.null ts = [T.singleton c]
    | otherwise = Prelude.init ts ++ [T.snoc (Prelude.last ts) c]

spl :: Text -> [Text]
spl = T.foldl' spl' [empty]

combine :: Int -> [Text] -> Text -> [Text]
combine width acc s = if nl then acc ++ [strip s] else Data.Taskell.Text.append (l `T.append` s) acc
    where l = if Prelude.null acc then empty else last acc
          nl = textWidth l + textWidth s > width

append :: Text -> [Text] -> [Text]
append s l = l' ++ [s]
    where l' = if Prelude.null l then l else Prelude.init l

wrap :: Int -> Text -> [Text]
wrap width = Data.List.foldl' (combine width) [empty] . spl
