module App (go) where

import Control.Monad (void)
import Flow.State (State, Mode(..), lists, continue, path, mode, current, size)
import Brick
import Brick.Types (Extent(..))
import Persistence.Taskell (writeFile)

import Flow.Actions (event)

import UI.Draw (draw, chooseCursor, colWidth, normalise)
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
        _ -> scroll s
handleEvent s _ = Brick.continue s

scroll :: State -> EventM ResourceName (Next State)
scroll s = do
    let (col, row) = current $ normalise s
        (w, h) = size s
    offset <- fmap sum . sequence $ fmap getHeight . lookupExtent . (\i -> RNTask (col, i)) <$> [0..row]
    setLeft (viewportScroll RNLists) $ (col * colWidth) - (w `div` 2 - colWidth `div` 2)
    setTop (viewportScroll (RNList col)) $ offset - h `div` 2
    Brick.continue s

getHeight :: Maybe (Extent ResourceName) -> Int
getHeight extent = case extent of
    Nothing -> 0
    Just (Extent _ _ (_, height) _) -> height

startEvent :: State -> EventM ResourceName State
startEvent = return

app :: App State e ResourceName
app = App draw chooseCursor handleEvent startEvent attrMap'

go :: State -> IO ()
go initial = void (defaultMain app initial)
