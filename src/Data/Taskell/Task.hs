{-# LANGUAGE DeriveGeneric #-}

module Data.Taskell.Task where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (isInfixOf)
import qualified Data.Taskell.String as S

newtype Task = Task {
    description :: String
} deriving (Generic, Show, Eq)

instance ToJSON Task
instance FromJSON Task

blank :: Task
blank = Task { description = "" }

clear :: Task -> Task
clear _ = blank

append :: Char -> Task  -> Task
append c t = t { description = description t ++ [c] }

backspace :: Task -> Task
backspace t = t { description = S.backspace (description t) }

characters :: Task -> Int
characters = length . description

contains :: String -> Task -> Bool
contains s t = s `isInfixOf` description t

isBlank :: Task -> Bool
isBlank t = null $ description t
