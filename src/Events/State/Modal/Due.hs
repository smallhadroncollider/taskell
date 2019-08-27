{-# LANGUAGE NoImplicitPrelude #-}

module Events.State.Modal.Due
    ( showDue
    , clearDate
    , previous
    , next
    , goto
    ) where

import ClassyPrelude

import Control.Lens  ((&), (.~), (^.))
import Data.Sequence ((!?))

import qualified Data.Taskell.Lists      as L (clearDue, due)
import           Events.State.Types      (Stateful, current, lists, mode)
import           Events.State.Types.Mode (ModalType (Due), Mode (..))

showDue :: Stateful
showDue state = do
    let due = L.due (state ^. lists)
    pure $ state & mode .~ Modal (Due due 0)

previous :: Stateful
previous state =
    case state ^. mode of
        Modal (Due due cur) -> do
            let pos =
                    if cur > 0
                        then cur - 1
                        else 0
            pure $ state & mode .~ Modal (Due due pos)
        _ -> pure state

next :: Stateful
next state =
    case state ^. mode of
        Modal (Due due cur) -> do
            let lim = length due - 1
            let pos =
                    if cur < lim
                        then cur + 1
                        else lim
            pure $ state & mode .~ Modal (Due due pos)
        _ -> pure state

goto :: Stateful
goto state =
    case state ^. mode of
        Modal (Due due cur) ->
            case due !? cur of
                Just (pointer, _) -> pure $ state & current .~ pointer
                Nothing           -> Nothing
        _ -> pure state

clearDate :: Stateful
clearDate state =
    case state ^. mode of
        Modal (Due due cur) ->
            case due !? cur of
                Just (pointer, _) -> do
                    let cleared = state & lists .~ L.clearDue pointer (state ^. lists)
                    pure $ cleared & mode .~ Modal (Due (L.due (cleared ^. lists)) 0)
                Nothing -> Nothing
        _ -> pure state
