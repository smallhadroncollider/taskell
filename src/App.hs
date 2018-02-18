module App (go) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Events.State (State, Mode(..), continue, path, mode, io)
import Data.Taskell.Lists (Lists)
import Brick

import IO.Taskell (writeFile)
import IO.Config (Config, layout, generateAttrMap)

import Events.Actions (event)

import UI.Draw (draw, chooseCursor)
import UI.Types (ResourceName(..))

-- store
store :: Config -> Lists -> State -> IO State
store config ls s = do
        forkIO $ IO.Taskell.writeFile config ls (path s)
        return (Events.State.continue s)

-- App code
handleEvent :: Config -> State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent config s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> Brick.halt s
        _ -> case io s of
            Just ls -> liftIO (store config ls s) >>= Brick.continue
            Nothing -> Brick.continue s
handleEvent _ s _ = Brick.continue s

go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    let app = App (draw $ layout config) chooseCursor (handleEvent config) return attrMap'
    void (defaultMain app initial)
