{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Taskell.Events.State.History
    ( undo
    , redo
    , store
    ) where

import ClassyPrelude

import Control.Lens (Lens', (&), (.~), (^.))

import Taskell.Events.State.Types (History (History), future, past, present)

λstack :: Lens' (History a) [a] -> History a -> [a]
λstack fn history = history ^. present : (history ^. fn)

store :: History a -> History a
store history = history & past .~ λstack past history & future .~ empty

undo :: History a -> History a
undo history =
    case history ^. past of
        []          -> history
        (moment:xs) -> History xs moment (λstack future history)

redo :: History a -> History a
redo history =
    case history ^. future of
        []          -> history
        (moment:xs) -> History (λstack past history) moment xs
