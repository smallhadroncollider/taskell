{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Events.State.History
    ( undo
    , redo
    , store
    ) where

import ClassyPrelude

import Control.Lens (Lens', (&), (.~), (^.))

import Events.State.Types (History, future, past, present)

λstack :: Lens' (History a) [a] -> History a -> [a]
λstack fn history = history ^. present : (history ^. fn)

store :: History a -> History a
store history = history & past .~ λstack past history & future .~ empty

undo :: History a -> History a
undo history =
    case history ^. past of
        []          -> history
        (moment:xs) -> history & present .~ moment & past .~ xs & future .~ λstack future history

redo :: History a -> History a
redo history =
    case history ^. future of
        []          -> history
        (moment:xs) -> history & present .~ moment & future .~ xs & past .~ λstack past history
