{-# LANGUAGE DeriveGeneric #-}

module Data.Taskell.Task where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

newtype Task = Task {
    description :: String
} deriving (Generic, Show, Eq)

instance ToJSON Task
instance FromJSON Task

blank :: Task
blank = Task { description = "" }

append :: Char -> Task  -> Task
append c t = t { description = description t ++ [c] }

backspace :: Task -> Task
backspace t = t { description = d' }
    where d = description t
          d' = if not (null d) then init d else d

characters :: Task -> Int
characters = length . description
