{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw.Types
    ( DrawState(..)
    , ReaderDrawState
    ) where

import ClassyPrelude

import Data.Taskell.Lists      (Lists)
import Events.State.Types      (Pointer)
import Events.State.Types.Mode (Mode)
import IO.Config.Layout        (Config)
import IO.Keyboard.Types       (Bindings)
import UI.Field                (Field)

data DrawState = DrawState
    { dsLists        :: Lists
    , dsMode         :: Mode
    , dsLayout       :: Config
    , dsPath         :: FilePath
    , dsToday        :: Day
    , dsCurrent      :: Pointer
    , dsField        :: Maybe Field
    , dsEditingTitle :: Bool
    , dsSearchTerm   :: Maybe Field
    , dsHeight       :: Int
    , dsBindings     :: Bindings
    }

-- | Use a Reader to pass around DrawState
type ReaderDrawState = ReaderT DrawState Identity
