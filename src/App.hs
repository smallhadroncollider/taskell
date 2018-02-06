module App (go) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Flow.State (State, Mode(..), lists, continue, path, mode, current, size, normalise)
import Brick
import Brick.Types (Extent(..))
import Persistence.Taskell (writeFile)

import Flow.Actions (event)

import UI.Draw (draw, chooseCursor, colWidth)
import UI.Attr (attrMap')
import UI.Types (ResourceName(..))

-- store
store :: State -> IO State
store s = do
        Persistence.Taskell.writeFile (lists s) (path s)
        return (Flow.State.continue s)

-- scroll
scroll :: State -> EventM ResourceName ()
scroll s = do
    let (col, row) = current $ normalise s
        (w, h) = size s
    offset <- fmap sum . sequence $ fmap getHeight . lookupExtent . (\i -> RNTask (col, i)) <$> [0..row]
    setLeft (viewportScroll RNLists) $ (col * colWidth) - (w `div` 2 - colWidth `div` 2)
    setTop (viewportScroll (RNList col)) $ offset - h `div` 2

getHeight :: Maybe (Extent ResourceName) -> Int
getHeight extent = case extent of
    Nothing -> 0
    Just (Extent _ _ (_, height) _) -> height

-- App code
handleEvent :: State -> BrickEvent ResourceName e -> EventM ResourceName (Next State)
handleEvent s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> Brick.halt s
        Write _ -> scroll s >> liftIO (store s) >>= Brick.continue
        _ -> scroll s >> Brick.continue s
handleEvent s _ = Brick.continue s

app :: App State e ResourceName
app = App draw chooseCursor handleEvent return attrMap'

go :: State -> IO ()
go initial = void (defaultMain app initial)
