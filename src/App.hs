{-# LANGUAGE NoImplicitPrelude #-}

module App
    ( go
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick
import Graphics.Vty              (Mode (BracketedPaste), outputIface, setMode, supportsMode)
import Graphics.Vty.Input.Events (Event (..))

import qualified Control.FoldDebounce as Debounce

import Data.Taskell.Date       (currentDay)
import Data.Taskell.Lists      (Lists)
import Events.Actions          (event)
import Events.State            (continue, countCurrent)
import Events.State.Types      (State, current, io, lists, mode, path)
import Events.State.Types.Mode (InsertMode (..), InsertType (..), ModalType (..), Mode (..))
import IO.Config               (Config, generateAttrMap, layout)
import IO.Taskell              (writeData)
import UI.Draw                 (chooseCursor, draw)
import UI.Types                (ResourceName (..))

type DebouncedMessage = (Lists, FilePath)

type DebouncedWrite = DebouncedMessage -> IO ()

type Trigger = Debounce.Trigger DebouncedMessage DebouncedMessage

-- store
store :: Config -> DebouncedMessage -> IO ()
store config (ls, pth) = writeData config ls pth

next :: DebouncedWrite -> State -> EventM ResourceName (Next State)
next send state =
    case state ^. io of
        Just ls -> do
            invalidateCache
            liftIO $ send (ls, state ^. path)
            Brick.continue $ Events.State.continue state
        Nothing -> Brick.continue state

-- debouncing
debounce :: Config -> State -> IO (DebouncedWrite, Trigger)
debounce config initial = do
    trigger <-
        Debounce.new
            Debounce.Args
            { Debounce.cb = store config
            , Debounce.fold = \_ b -> b
            , Debounce.init = (initial ^. lists, initial ^. path)
            }
            Debounce.def
    let send = Debounce.send trigger
    return (send, trigger)

-- cache clearing
clearCache :: State -> EventM ResourceName ()
clearCache state = do
    let (li, ti) = state ^. current
    invalidateCacheEntry (RNList li)
    invalidateCacheEntry (RNTask (li, ti))

clearAllTitles :: State -> EventM ResourceName ()
clearAllTitles state = do
    let count = length (state ^. lists)
    let range = [0 .. (count - 1)]
    void . sequence $ invalidateCacheEntry . RNList <$> range
    void . sequence $ invalidateCacheEntry . (\x -> RNTask (x, -1)) <$> range

clearList :: State -> EventM ResourceName ()
clearList state = do
    let (list, _) = state ^. current
    let count = countCurrent state
    let range = [0 .. (count - 1)]
    invalidateCacheEntry $ RNList list
    void . sequence $ invalidateCacheEntry . (\x -> RNTask (list, x)) <$> range

-- event handling
handleVtyEvent :: (DebouncedWrite, Trigger) -> State -> Event -> EventM ResourceName (Next State)
handleVtyEvent (send, trigger) previousState e = do
    let state = event e previousState
    case previousState ^. mode of
        Search _ _               -> invalidateCache
        (Modal MoveTo)           -> clearAllTitles previousState
        (Insert ITask ICreate _) -> clearList previousState
        _                        -> return ()
    case state ^. mode of
        Shutdown -> liftIO (Debounce.close trigger) >> Brick.halt state
        (Modal MoveTo) -> clearAllTitles state >> next send state
        (Insert ITask ICreate _) -> clearList state >> next send state
        _ -> clearCache previousState >> clearCache state >> next send state

handleEvent ::
       (DebouncedWrite, Trigger)
    -> State
    -> BrickEvent ResourceName e
    -> EventM ResourceName (Next State)
handleEvent _ state (VtyEvent (EvResize _ _)) = invalidateCache >> Brick.continue state
handleEvent db state (VtyEvent ev)            = handleVtyEvent db state ev
handleEvent _ state _                         = Brick.continue state

-- | Runs when the app starts
--   Adds paste support
appStart :: State -> EventM ResourceName State
appStart state = do
    vty <- getVtyHandle
    let output = outputIface vty
    when (supportsMode output BracketedPaste) $ liftIO $ setMode output BracketedPaste True
    return state

-- | Sets up Brick
go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    today <- currentDay
    db <- debounce config initial
    let app =
            App
            { appDraw = draw (layout config) today
            , appChooseCursor = chooseCursor
            , appHandleEvent = handleEvent db
            , appStartEvent = appStart
            , appAttrMap = attrMap'
            }
    void (defaultMain app initial)
