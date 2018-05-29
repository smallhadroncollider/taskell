{-# LANGUAGE NoImplicitPrelude #-}
module App (go) where

import ClassyPrelude
import Control.Concurrent (forkIO)

import Brick
import Graphics.Vty (Mode(BracketedPaste), outputIface, supportsMode, setMode)
import Graphics.Vty.Input.Events (Event(..))

import Data.Taskell.Lists (Lists)
import Data.Taskell.Date (currentDay)
import Events.Actions (event)
import Events.State (continue, countCurrent)
import Events.State.Types (State, Mode(..), ModalType(..), InsertMode(..), InsertType(..), path, mode, io, current, lists)
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

-- cache clearing
clearCache :: State -> EventM ResourceName ()
clearCache state = do
    let (li, ti) = current state
    invalidateCacheEntry (RNList li)
    invalidateCacheEntry (RNTask (li, ti))

clearAllTitles :: State -> EventM ResourceName ()
clearAllTitles state = do
    let count = length (lists state)
    let range = [0 .. (count - 1)]
    void . sequence $ invalidateCacheEntry . RNList <$> range
    void . sequence $ invalidateCacheEntry . (\x -> RNTask (x, -1)) <$> range

clearList :: State -> EventM ResourceName ()
clearList state = do
    let (list, _) = current state
    let count = countCurrent state
    let range = [0 .. (count - 1)]
    invalidateCacheEntry $ RNList list
    void . sequence $ invalidateCacheEntry . (\x -> RNTask (list, x)) <$> range

-- event handling
handleVtyEvent :: Config -> State -> Event -> EventM ResourceName (Next State)
handleVtyEvent config previousState e = do
    let state = event e previousState

    case mode previousState of
        Search _ _ -> invalidateCache
        (Modal MoveTo) -> clearAllTitles previousState
        (Insert ITask ICreate _) -> clearList previousState
        _ -> return ()

    case mode state of
        Shutdown -> Brick.halt state
        (Modal MoveTo) -> clearAllTitles state >> next config state
        (Insert ITask ICreate _) -> clearList state >> next config state
        _ -> clearCache previousState >> clearCache state >> next config state

handleEvent :: Config -> State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent _ state (VtyEvent (EvResize _ _ )) = invalidateCache >> Brick.continue state
handleEvent config state (VtyEvent ev) = handleVtyEvent config state ev
handleEvent _ state _ = Brick.continue state


-- | Runs when the app starts
--   Adds paste support
appStart :: State -> EventM ResourceName State
appStart state = do
    vty <- getVtyHandle
    let output = outputIface vty
    when (supportsMode output BracketedPaste) $
        liftIO $ setMode output BracketedPaste True
    return state

-- | Sets up Brick
go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    today <- currentDay
    let app = App {
            appDraw = draw (layout config) today
          , appChooseCursor = chooseCursor
          , appHandleEvent = handleEvent config
          , appStartEvent = appStart
          , appAttrMap = attrMap'
        }
    void (defaultMain app initial)
