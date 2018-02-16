module Data.Taskell.List where

import Data.Text (Text)

import Prelude hiding (splitAt, filter)
import Data.Sequence as S (Seq, (|>), (!?), (><), deleteAt, splitAt, filter, adjust', update, empty)
import qualified Data.Taskell.Seq as S

import Data.Taskell.Task (Task, blank, contains)

data List = List {
    title :: Text,
    tasks :: Seq Task
} deriving (Show, Eq)

-- useful functions
empty :: Text -> List
empty t = List {
    title = t,
    tasks = S.empty
}

new :: List -> List
new = append blank

count :: List -> Int
count = length . tasks

updateTitle :: List -> Text -> List
updateTitle ls s = ls { title = s }

newAt :: Int -> List -> List
newAt i l = l { tasks = (a |> blank) >< b }
    where (a, b) = splitAt i $ tasks l


append :: Task -> List -> List
append t l = l { tasks = tasks l |> t }

extract :: Int -> List -> Maybe (List, Task)
extract i l = do
    (xs, x) <- S.extract i (tasks l)
    return (l { tasks = xs }, x)

updateFn :: Int -> (Task -> Task) -> List -> List
updateFn i fn l = l { tasks = ts }
    where ts = adjust' fn i (tasks l)

update :: Int -> Task -> List -> List
update i t l = l { tasks = ts }
    where ts = S.update i t (tasks l)

move :: Int -> Int -> List -> Maybe List
move from dir l = do
    ts' <- S.shiftBy from dir (tasks l)
    return $ l { tasks = ts' }

deleteTask :: Int -> List -> List
deleteTask i l = l { tasks = deleteAt i (tasks l) }

getTask :: Int -> List -> Maybe Task
getTask i l = tasks l !? i

searchFor :: Text -> List -> List
searchFor s l = l { tasks = filter (contains s) (tasks l)}
