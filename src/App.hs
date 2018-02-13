module App (go) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Events.State (State, Mode(..), lists, continue, path, mode)
import Brick

import IO.Taskell (writeFile)
import IO.Config (Config, layout, generateAttrMap)

import Events.Actions (event)

import UI.Draw (draw, chooseCursor)
import UI.Types (ResourceName(..))

-- store
store :: State -> IO State
store s = do
        forkIO $ IO.Taskell.writeFile (lists s) (path s)
        return (Events.State.continue s)

-- App code
handleEvent :: State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> Brick.halt s
        Write _ -> liftIO (store s) >>= Brick.continue
        _ -> Brick.continue s
handleEvent s _ = Brick.continue s

go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    let app = App (draw $ layout config) chooseCursor handleEvent return attrMap'
    void (defaultMain app initial)
