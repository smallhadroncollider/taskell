module App (go) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Events.State (State, Mode(..), continue, path, mode, io, current)
import Data.Taskell.Lists (Lists)
import Brick
import Graphics.Vty.Input.Events (Event(..))

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

next :: Config -> State -> EventM ResourceName (Next State)
next config s = case io s of
    Just ls -> liftIO (store config ls s) >>= Brick.continue
    Nothing -> Brick.continue s

clearCache :: State -> EventM ResourceName ()
clearCache state = do
    let (li, ti) = current state
    invalidateCacheEntry (RNList li)
    invalidateCacheEntry (RNTask (li, ti))

-- App code
handleEvent :: Config -> State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent _ s (VtyEvent (EvResize _ _ )) = invalidateCache >> Brick.continue s
handleEvent config s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> Brick.halt s
        _ -> clearCache s' >> clearCache s >> next config s
handleEvent _ s _ = Brick.continue s

go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    let app = App (draw $ layout config) chooseCursor (handleEvent config) return attrMap'
    void (defaultMain app initial)
