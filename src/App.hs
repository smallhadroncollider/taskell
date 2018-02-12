module App (go) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Flow.State (State, Mode(..), lists, continue, path, mode)
import Brick
import Persistence.Taskell (writeFile)

import Flow.Actions (event)

import UI.Draw (draw, chooseCursor, scroll)
import UI.Theme (generateAttrMap)
import UI.Types (ResourceName(..))

-- store
store :: State -> IO State
store s = do
        forkIO $ Persistence.Taskell.writeFile (lists s) (path s)
        return (Flow.State.continue s)

-- App code
handleEvent :: State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> Brick.halt s
        Write _ -> scroll s >> liftIO (store s) >>= Brick.continue
        _ -> scroll s >> Brick.continue s
handleEvent s _ = Brick.continue s

go :: State -> IO ()
go initial = do
    attrMap' <- const <$> generateAttrMap
    let app = App draw chooseCursor handleEvent return attrMap'
    void (defaultMain app initial)
