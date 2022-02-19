module Taskell.Events.Actions
    ( event
    , generateActions
    , ActionSets
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Graphics.Vty.Input.Events (Event (..))

import Taskell.Events.State.Types      (State, Stateful, mode)
import Taskell.Events.State.Types.Mode (DetailMode (..), ModalType (..), Mode (..))

import Taskell.IO.Keyboard       (generate)
import Taskell.IO.Keyboard.Types (Bindings, BoundActions)

import qualified Taskell.Events.Actions.Insert             as Insert
import qualified Taskell.Events.Actions.Modal              as Modal
import qualified Taskell.Events.Actions.Modal.Detail       as Detail
import qualified Taskell.Events.Actions.Modal.Due          as Due
import qualified Taskell.Events.Actions.Modal.Help         as Help
import qualified Taskell.Events.Actions.Normal             as Normal
import qualified Taskell.Events.Actions.Search             as Search
import qualified Taskell.Events.Actions.Modal.TaskList     as TaskList

-- takes an event and returns a Maybe State
event' :: Event -> Stateful
-- for other events pass through to relevant modules
event' e state =
    case state ^. mode of
        Normal    -> Normal.event e state
        Search    -> Search.event e state
        Insert {} -> Insert.event e state
        Modal {}  -> Modal.event e state
        _         -> pure state

-- returns new state if successful
event :: ActionSets -> Event -> State -> State
event actions e state = do
    let mEv =
            case state ^. mode of
                Normal                        -> lookup e $ normal actions
                Modal (Detail _ DetailNormal) -> lookup e $ detail actions
                Modal Due {}                  -> lookup e $ due actions
                Modal (Help _)                -> lookup e $ help actions
                Modal TaskList {}             -> lookup e $ taskList actions
                _                             -> Nothing
    fromMaybe state $
        case mEv of
            Nothing -> event' e state
            Just ev -> ev state

data ActionSets = ActionSets
    { normal :: BoundActions
    , detail :: BoundActions
    , help   :: BoundActions
    , due    :: BoundActions
    , taskList :: BoundActions
    }

generateActions :: Bindings -> ActionSets
generateActions bindings =
    ActionSets
    { normal = generate bindings Normal.events
    , detail = generate bindings Detail.events
    , help = generate bindings Help.events
    , due = generate bindings Due.events
    , taskList = generate bindings TaskList.events
    }
