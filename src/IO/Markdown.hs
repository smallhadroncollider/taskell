module IO.Markdown (
    parse,
    stringify
) where

import Data.Text (Text, drop, append, null, lines, isPrefixOf, strip)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)

import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (List, title, tasks)
import Data.Taskell.Task (Task, new, description)
import Data.Foldable (foldl')
import Data.Sequence (empty)
import Data.ByteString (ByteString)
import Data.Word (Word8)

-- parse code
trimTitle :: Text -> Text
trimTitle = strip . Data.Text.drop 2

trimTask :: Text -> Task
trimTask = new . strip . Data.Text.drop 1

start :: Lists -> Text -> Lists
start ls s | "##" `isPrefixOf` s = newList (trimTitle s) ls
           | "-" `isPrefixOf` s = appendToLast (trimTask s) ls
           | otherwise = ls

decodeError :: String -> Maybe Word8 -> Maybe Char
decodeError _ _ = Just '\65533'

parse :: ByteString -> Lists
parse s = foldl' start empty $ Data.Text.lines $ decodeUtf8With decodeError s

-- stringify code
join :: Text -> [Text] -> Text
join = foldl' Data.Text.append

taskToString :: Text -> Task -> Text
taskToString s t = join s ["- ", description t, "\n"]

listToString :: Text -> List -> Text
listToString s l = join s [
        if Data.Text.null s then "" else "\n"
      , "## "
      , title l
      , "\n\n"
      , foldl' taskToString "" (tasks l)
    ]

stringify :: Lists -> ByteString
stringify ls = encodeUtf8 $ foldl' listToString "" ls
