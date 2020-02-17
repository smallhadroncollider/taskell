module Taskell.Events.State.Modal.Due
    ( showDue
    , clearDate
    , previous
    , next
    , goto
    ) where

import ClassyPrelude

import Control.Lens  ((&), (.~), (^.))
import Data.Sequence ((!?))

import qualified Taskell.Data.Lists              as L (clearDue, due)
import           Taskell.Data.Seq                (bound)
import           Taskell.Data.Task               (Task)
import           Taskell.Events.State.Types      (Stateful, current, lists, mode)
import           Taskell.Events.State.Types.Mode (ModalType (Due), Mode (..))
import           Taskell.Types                   (Pointer)

type DueStateful = Seq (Pointer, Task) -> Int -> Stateful

λfilter :: DueStateful -> Stateful
λfilter fn state =
    case state ^. mode of
        Modal (Due due cur) -> fn due cur state
        _                   -> pure state

λsetMode :: DueStateful
λsetMode due pos state = pure $ state & mode .~ Modal (Due due pos)

λprevious :: DueStateful
λprevious due cur = λsetMode due (bound due (cur - 1))

λnext :: DueStateful
λnext due cur = λsetMode due (bound due (cur + 1))

λgoto :: DueStateful
λgoto due cur state =
    case due !? cur of
        Just (pointer, _) -> pure $ state & current .~ pointer
        Nothing           -> Nothing

λclearDate :: DueStateful
λclearDate due cur state =
    case due !? cur of
        Just (pointer, _) -> do
            let new = L.clearDue pointer (state ^. lists)
            let dues = L.due new
            λsetMode dues (bound dues cur) (state & lists .~ new)
        Nothing -> Nothing

showDue :: Stateful
showDue state = λsetMode (L.due (state ^. lists)) 0 state

previous :: Stateful
previous = λfilter λprevious

next :: Stateful
next = λfilter λnext

goto :: Stateful
goto = λfilter λgoto

clearDate :: Stateful
clearDate = λfilter λclearDate
