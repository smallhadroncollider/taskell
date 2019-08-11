{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw.Types
    ( DrawState(..)
    , ReaderDrawState
    ) where

import ClassyPrelude

import Events.State.Types (State)
import IO.Config.Layout   (Config)
import IO.Keyboard.Types  (Bindings)

data DrawState = DrawState
    { dsLayout   :: Config
    , dsBindings :: Bindings
    , dsToday    :: Day
    , dsState    :: State
    }

-- | Use a Reader to pass around DrawState
type ReaderDrawState = ReaderT DrawState Identity
