module App (go) where

import Control.Monad (void)
import Flow.State (State, Mode(..), lists, continue, path, mode, current)
import Brick
import Brick.Types (Extent(..))
import Persistence.Taskell (writeFile)

import Flow.Actions (event)

import UI.Draw (draw, chooseCursor, colWidth)
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
        _ -> do
            let cur = current s
                list = fst cur
                view = viewportScroll RNLists
                stop = snd cur - 1
                resources = (\i -> RNTask (list, i)) <$> [0..stop]

            offset <- fmap sum . sequence $ (getHeight <$>) . lookupExtent <$> resources

            setLeft view (list * colWidth)
            setTop (viewportScroll (RNList list)) offset

            Brick.continue s

handleEvent s _ = Brick.continue s

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
