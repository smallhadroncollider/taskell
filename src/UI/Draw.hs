{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw
    ( draw
    , chooseCursor
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Control.Monad.Reader (runReader)

import Brick

import Data.Taskell.Date       (Day)
import Events.State            (normalise)
import Events.State.Types      (State, mode)
import Events.State.Types.Mode (DetailMode (..), ModalType (..), Mode (..))
import IO.Config.Layout        (Config)
import IO.Keyboard.Types       (Bindings)
import UI.Draw.Main            (renderMain)
import UI.Draw.Modal           (renderModal)
import UI.Draw.Types           (DrawState (DrawState), ReaderDrawState, TWidget)
import UI.Types                (ResourceName (..))

-- draw
renderApp :: ReaderDrawState [TWidget]
renderApp = sequence [renderModal, renderMain]

draw :: Config -> Bindings -> Day -> Bool -> State -> [TWidget]
draw layout bindings today debug state =
    runReader renderApp (DrawState layout bindings today debug (normalise state))

-- cursors
chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state =
    case normalise state ^. mode of
        Insert {}                         -> showCursorNamed RNCursor
        Search                            -> showCursorNamed RNCursor
        Modal (Detail _ (DetailInsert _)) -> showCursorNamed RNCursor
        _                                 -> neverShowCursor state
