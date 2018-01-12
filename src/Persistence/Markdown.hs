module Persistence.Markdown (
    parse,
    stringify
) where

import Prelude hiding (lines)

import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (List, title, tasks)
import Data.Taskell.Task (Task, new, description)
import Data.Taskell.String (trim)
import Data.Foldable (foldl')
import Data.Sequence (empty)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (lines, isPrefixOf, pack, unpack)

-- parse code
trimTitle :: ByteString -> String
trimTitle s = trim . drop 2 $ unpack s

trimTask :: ByteString -> Task
trimTask = new . trim . drop 1 . unpack

start :: Lists -> ByteString -> Lists
start ls s | pack "##" `isPrefixOf` s  = newList (trimTitle s) ls
           | pack "-" `isPrefixOf` s  = appendToLast (trimTask s) ls
           | otherwise = ls

parse :: ByteString -> Lists
parse s = foldl' start empty $ lines s

-- stringify code
join :: String -> [String] -> String
join = foldl' (++)

taskToString :: String -> Task -> String
taskToString s t = join s ["- ", description t, "\n"]

listToString :: String -> List -> String
listToString s l = join s [
        if null s then "" else "\n"
      , "## "
      , title l
      , "\n\n"
      , foldl' taskToString "" (tasks l)
    ]

stringify :: Lists -> ByteString
stringify ls = pack $ foldl' listToString "" ls
