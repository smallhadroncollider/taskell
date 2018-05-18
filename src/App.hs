{-# LANGUAGE NoImplicitPrelude #-}
module App (go) where

import ClassyPrelude
import Control.Concurrent (forkIO)

import Brick
import Graphics.Vty (Mode(BracketedPaste), outputIface, supportsMode, setMode)
import Graphics.Vty.Input.Events (Event(..))

import Data.Taskell.Lists (Lists)
import Data.Taskell.Date (currentDay, deadline)
import Events.Actions (event)
import Events.State (State, Mode(..), continue, path, mode, io, current)
import IO.Config (Config, layout, generateAttrMap)
import IO.Taskell (writeData)
import UI.Draw (draw, chooseCursor)
import UI.Types (ResourceName(..))

-- store
store :: Config -> Lists -> State -> IO State
store config ls s = do
    _ <- forkIO $ writeData config ls (path s)
    return (Events.State.continue s)

next :: Config -> State -> EventM ResourceName (Next State)
next config s = case io s of
    Just ls -> invalidateCache >> liftIO (store config ls s) >>= Brick.continue
    Nothing -> Brick.continue s

clearCache :: State -> EventM ResourceName ()
clearCache state = do
    let (li, ti) = current state
    invalidateCacheEntry (RNList li)
    invalidateCacheEntry (RNTask (li, ti))

handleVtyEvent :: Config -> State -> Event -> EventM ResourceName (Next State)
handleVtyEvent config previousState e = do
    let state = event e previousState

    case mode previousState of
        Search _ _ -> invalidateCache
        _ -> return  ()

    case mode state of
        Shutdown -> Brick.halt state
        _ -> clearCache previousState >> clearCache state >> next config state

-- App code
handleEvent :: Config -> State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent _ state (VtyEvent (EvResize _ _ )) = invalidateCache >> Brick.continue state
handleEvent config state (VtyEvent ev) = handleVtyEvent config state ev
handleEvent _ state _ = Brick.continue state

appStart :: State -> EventM ResourceName State
appStart state = do
    vty <- getVtyHandle
    let output = outputIface vty
    when (supportsMode output BracketedPaste) $
        liftIO $ setMode output BracketedPaste True
    return state

go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    deadlineFn <- deadline . Just <$> currentDay
    let app = App {
            appDraw = draw (layout config) deadlineFn
          , appChooseCursor = chooseCursor
          , appHandleEvent = handleEvent config
          , appStartEvent = appStart
          , appAttrMap = attrMap'
        }
    void (defaultMain app initial)
