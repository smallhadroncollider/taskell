module Persistence.Markdown (
    parse,
    stringify
) where

import Prelude hiding (lines)

import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (List, empty, title, tasks)
import Data.Taskell.Task (Task, new, description)
import Data.Taskell.String (trim)
import Data.Foldable (foldl')
import Data.Sequence (Seq, fromList)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (lines, isPrefixOf, pack, unpack)

trimTitle :: ByteString -> String
trimTitle s = trim . drop 2 $ unpack s

trimTask :: ByteString -> Task
trimTask = new . trim . drop 1 . unpack

start :: Lists -> ByteString -> Lists
start ls s | pack "##" `isPrefixOf` s  = newList (trimTitle s) ls
           | pack "-" `isPrefixOf` s  = appendToLast (trimTask s) ls
           | otherwise = ls

parse :: ByteString -> Lists
parse s = foldl' start (fromList []) $ lines s

taskToString :: String -> Task -> String
taskToString s t = s ++ "- " ++ description t ++ "\n"

listToString :: String -> List -> String
listToString s l = s ++ (if null s then "" else "\n") ++ "## " ++ title l ++ "\n\n" ++ foldl' taskToString "" (tasks l)

stringify :: Lists -> ByteString
stringify ls = pack $ foldl' listToString "" ls
