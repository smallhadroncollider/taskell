{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw.Types where

import ClassyPrelude

import Brick              (Widget)
import Events.State.Types (State)
import IO.Config.Layout   (Config)
import IO.Keyboard.Types  (Bindings)
import UI.Types           (ResourceName)

data DrawState = DrawState
    { dsLayout   :: Config
    , dsBindings :: Bindings
    , dsDebug    :: Bool
    , dsState    :: State
    }

-- | Use a Reader to pass around DrawState
type ReaderDrawState = ReaderT DrawState Identity

-- | Aliases for common combinations
type TWidget = Widget ResourceName

type ModalWidget = ReaderDrawState (Text, TWidget)

type DSWidget = ReaderDrawState TWidget
