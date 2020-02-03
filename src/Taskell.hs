{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Taskell
    ( go
    ) where

import ClassyPrelude

import Control.Concurrent (forkIO, threadDelay)

import Control.Lens ((^.))

import Data.Time.Zones (TZ)

import Brick
import Brick.BChan               (BChan, newBChan, writeBChan)
import Graphics.Vty              (Mode (BracketedPaste), defaultConfig, displayBounds, mkVty,
                                  outputIface, setMode, supportsMode)
import Graphics.Vty.Input.Events (Event (..))

import qualified Control.FoldDebounce as Debounce

import Taskell.Data.Lists              (Lists)
import Taskell.Events.Actions          (ActionSets, event, generateActions)
import Taskell.Events.State            (continue, countCurrent, setHeight, setTime)
import Taskell.Events.State.Types      (State, current, io, lists, mode, path, searchTerm, timeZone)
import Taskell.Events.State.Types.Mode (InsertMode (..), InsertType (..), ModalType (..), Mode (..))
import Taskell.IO                      (writeData)
import Taskell.IO.Config               (Config, debugging, generateAttrMap, getBindings, layout)
import Taskell.Types                   (ListIndex (..), TaskIndex (..))
import Taskell.UI.Draw                 (chooseCursor, draw)
import Taskell.UI.Types                (ResourceName (..))

type DebouncedMessage = (Lists, FilePath, TZ)

type DebouncedWrite = DebouncedMessage -> IO ()

type Trigger = Debounce.Trigger DebouncedMessage DebouncedMessage

-- tick
data TaskellEvent =
    Tick

oneSecond :: Int
oneSecond = 1000000

frequency :: Int
frequency = 60 * oneSecond

timer :: BChan TaskellEvent -> IO ()
timer chan =
    void . forkIO . forever $ do
        writeBChan chan Tick
        threadDelay frequency

-- store
store :: Config -> DebouncedMessage -> IO ()
store config (ls, pth, tz) = writeData tz config ls pth

next :: DebouncedWrite -> State -> EventM ResourceName (Next State)
next send state =
    case state ^. io of
        Just ls -> do
            invalidateCache
            liftIO $ send (ls, state ^. path, state ^. timeZone)
            Brick.continue $ Taskell.Events.State.continue state
        Nothing -> Brick.continue state

-- debouncing
debounce :: Config -> State -> IO (DebouncedWrite, Trigger)
debounce config initial = do
    trigger <-
        Debounce.new
            Debounce.Args
            { Debounce.cb = store config
            , Debounce.fold = flip const
            , Debounce.init = (initial ^. lists, initial ^. path, initial ^. timeZone)
            }
            Debounce.def
    let send = Debounce.send trigger
    pure (send, trigger)

-- cache clearing
clearCache :: State -> EventM ResourceName ()
clearCache state = do
    let (ListIndex li, TaskIndex ti) = state ^. current
    invalidateCacheEntry (RNList li)
    invalidateCacheEntry (RNTask (ListIndex li, TaskIndex ti))

clearAllTitles :: State -> EventM ResourceName ()
clearAllTitles state = do
    let count = length (state ^. lists)
    let range = [0 .. (count - 1)]
    traverse_ (invalidateCacheEntry . RNList) range
    traverse_ (invalidateCacheEntry . RNTask . (, TaskIndex (-1)) . ListIndex) range

clearList :: State -> EventM ResourceName ()
clearList state = do
    let (ListIndex list, _) = state ^. current
    let count = countCurrent state
    let range = [0 .. (count - 1)]
    invalidateCacheEntry $ RNList list
    traverse_ (invalidateCacheEntry . RNTask . (,) (ListIndex list) . TaskIndex) range

clearDue :: State -> EventM ResourceName ()
clearDue state =
    case state ^. mode of
        Modal (Due dues _) -> do
            let range = [0 .. (length dues + 1)]
            traverse_ (invalidateCacheEntry . RNDue) range
        _ -> pure ()

-- event handling
handleVtyEvent ::
       (DebouncedWrite, Trigger) -> ActionSets -> State -> Event -> EventM ResourceName (Next State)
handleVtyEvent (send, trigger) actions previousState e = do
    let state = event actions e previousState
    when (previousState ^. searchTerm /= state ^. searchTerm) invalidateCache
    case previousState ^. mode of
        (Modal MoveTo)           -> clearAllTitles previousState
        (Insert ITask ICreate _) -> clearList previousState
        _                        -> pure ()
    case state ^. mode of
        Shutdown -> liftIO (Debounce.close trigger) *> Brick.halt state
        (Modal Due {}) -> clearDue state *> next send state
        (Modal MoveTo) -> clearAllTitles state *> next send state
        (Insert ITask ICreate _) -> clearList state *> next send state
        _ -> clearCache previousState *> clearCache state *> next send state

getHeight :: EventM ResourceName Int
getHeight = snd <$> (liftIO . displayBounds =<< outputIface <$> getVtyHandle)

handleEvent ::
       (DebouncedWrite, Trigger)
    -> ActionSets
    -> State
    -> BrickEvent ResourceName TaskellEvent
    -> EventM ResourceName (Next State)
handleEvent _ _ state (AppEvent Tick) = do
    t <- liftIO getCurrentTime
    Brick.continue $ setTime t state
handleEvent _ _ state (VtyEvent (EvResize _ _)) = do
    invalidateCache
    h <- getHeight
    Brick.continue (setHeight h state)
handleEvent db actions state (VtyEvent ev) = handleVtyEvent db actions state ev
handleEvent _ _ state _ = Brick.continue state

-- | Runs when the app starts
--   Adds paste support
appStart :: State -> EventM ResourceName State
appStart state = do
    output <- outputIface <$> getVtyHandle
    when (supportsMode output BracketedPaste) . liftIO $ setMode output BracketedPaste True
    h <- getHeight
    pure (setHeight h state)

-- | Sets up Brick
go :: Config -> State -> IO ()
go config initial = do
    attrMap' <- const <$> generateAttrMap
    -- setup debouncing
    db <- debounce config initial
    -- get bindings
    bindings <- getBindings
    -- setup timer channel
    timerChan <- newBChan 1
    timer timerChan
    -- create app
    let app =
            App
            { appDraw = draw (layout config) bindings (debugging config)
            , appChooseCursor = chooseCursor
            , appHandleEvent = handleEvent db (generateActions bindings)
            , appStartEvent = appStart
            , appAttrMap = attrMap'
            }
    -- start
    let builder = mkVty defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just timerChan) app initial
