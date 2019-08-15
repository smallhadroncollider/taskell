{-# LANGUAGE NoImplicitPrelude #-}

module Events.State.Modal.Due
    ( showDue
    , previous
    , next
    , goto
    ) where

import ClassyPrelude

import Control.Lens ((&), (.~), (^.))

import qualified Data.Taskell.Lists      as L (due)
import           Events.State.Types      (Stateful, lists, mode)
import           Events.State.Types.Mode (ModalType (Due), Mode (..))

showDue :: Stateful
showDue state = do
    let due = L.due (state ^. lists)
    pure $ state & mode .~ Modal (Due due 0)

previous :: Stateful
previous state =
    case state ^. mode of
        Modal (Due due current) -> do
            let pos =
                    if current > 0
                        then current - 1
                        else 0
            pure $ state & mode .~ Modal (Due due pos)
        _ -> pure state

next :: Stateful
next state =
    case state ^. mode of
        Modal (Due due current) -> do
            let lim = length due - 1
            let pos =
                    if current < lim
                        then current + 1
                        else lim
            pure $ state & mode .~ Modal (Due due pos)
        _ -> pure state

goto :: Stateful
goto = pure
