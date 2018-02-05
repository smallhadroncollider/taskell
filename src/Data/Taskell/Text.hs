module Data.Taskell.Text (
    backspace,
    wrap
) where

import Data.Text (Text, foldl', singleton, snoc, init, null, append, strip, empty)
import Data.List (foldl')
import Brick.Widgets.Core (textWidth)

backspace :: Text -> Text
backspace s = if not (Data.Text.null s) then Data.Text.init s else s

-- wrapping
spl' :: [Text] -> Char -> [Text]
spl' ts c
    | c == ' ' = ts ++ [" "] ++ [empty]
    | Prelude.null ts = [Data.Text.singleton c]
    | otherwise = Prelude.init ts ++ [Data.Text.snoc (Prelude.last ts) c]

spl :: Text -> [Text]
spl = Data.Text.foldl' spl' [empty]

wrap :: Int -> Text -> [Text]
wrap width = Data.List.foldl' (combine width) [] . spl

combine :: Int -> [Text] -> Text -> [Text]
combine width acc s = if nl then acc ++ [strip s] else Data.Taskell.Text.append (l `Data.Text.append` s) acc
    where l = if Prelude.null acc then "" else last acc
          nl = textWidth l + textWidth s > width

append :: Text -> [Text] -> [Text]
append s l = l' ++ [s]
    where l' = if Prelude.null l then l else Prelude.init l
