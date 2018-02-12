module App (go) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Events.State (State, Mode(..), lists, continue, path, mode)
import Brick

import IO.Taskell (writeFile)
import IO.Config (Config, LayoutConfig, layout, generateAttrMap)

import Events.Actions (event)

import UI.Draw (draw, chooseCursor, scroll)
import UI.Types (ResourceName(..))

-- store
store :: State -> IO State
store s = do
        forkIO $ IO.Taskell.writeFile (lists s) (path s)
        return (Events.State.continue s)

-- App code
handleEvent :: LayoutConfig -> State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent lo s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> Brick.halt s
        Write _ -> scroll lo s >> liftIO (store s) >>= Brick.continue
        _ -> scroll lo s >> Brick.continue s
handleEvent _ s _ = Brick.continue s

go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    let app = App (draw $ layout config) chooseCursor (handleEvent $ layout config) return attrMap'
    void (defaultMain app initial)
