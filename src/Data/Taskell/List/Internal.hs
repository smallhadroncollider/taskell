{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Taskell.List.Internal where

import ClassyPrelude

import Control.Lens (ix, makeLenses, (%%~), (%~), (&), (.~), (^.), (^?))

import Data.Sequence as S (adjust', deleteAt, insertAt, update, (|>))

import qualified Data.Taskell.Seq  as S
import qualified Data.Taskell.Task as T (Task, Update, blank, contains)

data List = List
    { _title :: Text
    , _tasks :: Seq T.Task
    } deriving (Show, Eq)

type Update = List -> List

-- create lenses
$(makeLenses ''List)

-- operations
create :: Text -> Seq T.Task -> List
create = List

empty :: Text -> List
empty text = List text ClassyPrelude.empty

new :: Update
new = append T.blank

count :: List -> Int
count = length . (^. tasks)

newAt :: Int -> Update
newAt idx = tasks %~ insertAt idx T.blank

append :: T.Task -> Update
append task = tasks %~ (|> task)

extract :: Int -> List -> Maybe (List, T.Task)
extract idx list = do
    (xs, x) <- S.extract idx (list ^. tasks)
    return (list & tasks .~ xs, x)

updateFn :: Int -> T.Update -> Update
updateFn idx fn = tasks %~ adjust' fn idx

update :: Int -> T.Task -> Update
update idx task = tasks %~ S.update idx task

move :: Int -> Int -> List -> Maybe List
move from dir = tasks %%~ S.shiftBy from dir

deleteTask :: Int -> Update
deleteTask idx = tasks %~ deleteAt idx

getTask :: Int -> List -> Maybe T.Task
getTask idx = (^? tasks . ix idx)

searchFor :: Text -> Update
searchFor text = tasks %~ filter (T.contains text)
