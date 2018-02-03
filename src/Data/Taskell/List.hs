module Data.Taskell.List where

import Data.Text (Text)

import Prelude hiding (splitAt, filter)
import Data.Sequence (Seq, (|>), (!?), (><), deleteAt, splitAt, filter)
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

update :: Int -> (Task -> Task) -> List -> Maybe List
update i fn l = do
    ts' <- S.updateFn i fn (tasks l)
    return $ l { tasks = ts' }

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
