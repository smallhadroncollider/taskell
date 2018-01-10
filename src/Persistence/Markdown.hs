module Persistence.Markdown (parse) where

import Prelude hiding (lines, null)

import Data.Taskell.Lists (Lists, newList, appendToLast)
import Data.Taskell.List (empty)
import Data.Taskell.Task (Task, new)
import Data.Taskell.String (trim)
import Data.List (foldl')
import Data.Sequence (Seq, fromList)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (lines, null, isPrefixOf, pack, unpack)

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
