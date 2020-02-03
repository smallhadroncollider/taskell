{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.HTTP.Trello.List
    ( List
    , cards
    , setCards
    , listToList
    ) where

import ClassyPrelude

import Control.Lens (makeLenses, (&), (.~), (^.))

import Taskell.IO.HTTP.Aeson       (deriveFromJSON)
import Taskell.IO.HTTP.Trello.Card (Card, cardToTask)

import qualified Taskell.Data.List as L (List, create)

data List = List
    { _name  :: Text
    , _cards :: [Card]
    } deriving (Eq, Show)

-- create Aeson code
$(deriveFromJSON ''List)

-- create lenses
$(makeLenses ''List)

-- operations
setCards :: List -> [Card] -> List
setCards list cs = list & cards .~ cs

listToList :: List -> L.List
listToList ls = L.create (ls ^. name) (fromList $ cardToTask <$> (ls ^. cards))
