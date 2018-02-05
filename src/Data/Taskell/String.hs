module Data.Taskell.String (
    backspace,
    wrap,
    trim
) where

import Prelude hiding (words, unwords, length, take)
import Data.Text (Text, foldl', singleton, snoc, unwords, words, length, take, init, null, append)
import Data.List (foldl')
import Brick.Widgets.Core (textWidth)

backspace :: Text -> Text
backspace s = if not (Data.Text.null s) then Data.Text.init s else s

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
          nl = textWidth l + textWidth s > width

append :: Text -> [Text] -> [Text]
append s l = l' ++ [s]
    where l' = if Prelude.null l then l else Prelude.init l
