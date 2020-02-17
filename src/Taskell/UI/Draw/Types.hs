module Taskell.UI.Draw.Types where

import ClassyPrelude

import Brick (Widget)

import Taskell.Events.State.Types (State)
import Taskell.IO.Config.Layout   (Config)
import Taskell.IO.Keyboard.Types  (Bindings)
import Taskell.UI.Types           (ResourceName)

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
