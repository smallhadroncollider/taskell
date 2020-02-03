{-# LANGUAGE NoImplicitPrelude #-}

module Taskell.UI.Draw
    ( draw
    , chooseCursor
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Control.Monad.Reader (runReader)

import Brick

import Taskell.Events.State            (normalise)
import Taskell.Events.State.Types      (State, mode)
import Taskell.Events.State.Types.Mode (DetailMode (..), ModalType (..), Mode (..))
import Taskell.IO.Config.Layout        (Config)
import Taskell.IO.Keyboard.Types       (Bindings)
import Taskell.UI.Draw.Main            (renderMain)
import Taskell.UI.Draw.Modal           (renderModal)
import Taskell.UI.Draw.Types           (DrawState (DrawState), ReaderDrawState, TWidget)
import Taskell.UI.Types                (ResourceName (..))

-- draw
renderApp :: ReaderDrawState [TWidget]
renderApp = sequence [renderModal, renderMain]

draw :: Config -> Bindings -> Bool -> State -> [TWidget]
draw layout bindings debug state =
    runReader renderApp (DrawState layout bindings debug (normalise state))

-- cursors
chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state =
    case normalise state ^. mode of
        Insert {}                         -> showCursorNamed RNCursor
        Search                            -> showCursorNamed RNCursor
        Modal (Detail _ (DetailInsert _)) -> showCursorNamed RNCursor
        _                                 -> neverShowCursor state
