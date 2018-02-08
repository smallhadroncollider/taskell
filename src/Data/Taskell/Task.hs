module Data.Taskell.Task where

import Data.Text (Text, snoc, length, null, isInfixOf)
import qualified Data.Taskell.Text as T

newtype Task = Task {
    description :: Text
} deriving (Show, Eq)

blank :: Task
blank = Task { description = "" }

clear :: Task -> Task
clear _ = blank

new :: Text -> Task
new s = Task { description = s }

append :: Char -> Task  -> Task
append c t = t { description = Data.Text.snoc (description t) c }

backspace :: Task -> Task
backspace t = t { description = T.backspace (description t) }

characters :: Task -> Int
characters = Data.Text.length . description

contains :: Text -> Task -> Bool
contains s t = s `Data.Text.isInfixOf` description t

isBlank :: Task -> Bool
isBlank t = Data.Text.null $ description t
