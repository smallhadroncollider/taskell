module App (go) where

import Control.Monad (void)
import Flow.State (State, Mode(..), lists, continue, path, mode)
import Brick
import Persistence.Taskell (writeFile)

import Flow.Actions (event)

import UI.Draw (draw, chooseCursor)
import UI.Attr (attrMap')
import UI.Types (ResourceName(..))

-- app
handleEvent :: State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> halt s
        Write _ -> suspendAndResume $ do
            Persistence.Taskell.writeFile (lists s) (path s)
            return (Flow.State.continue s)
        _ -> do
            Brick.continue s
handleEvent s _ = Brick.continue s

startEvent :: State -> EventM ResourceName State
startEvent = return

app :: App State e ResourceName
app = App draw chooseCursor handleEvent startEvent attrMap'

go :: State -> IO ()
go initial = void (defaultMain app initial)
