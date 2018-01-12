module App (go) where

import Control.Monad (void)
import Flow.State (State, Mode(..), lists, current, mode, continue, path)
import Brick
import Brick.Util (fg)
import Graphics.Vty (defAttr, green)
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description)
import Data.Taskell.String (wrap)
import Data.Foldable (toList)
import Data.Sequence (mapWithIndex)
import Persistence.Taskell (writeFile)

import Flow.Actions (event)

import Config

data Name = ListLocation (Int, Int) | Empty deriving (Eq, Ord)

titleAttr :: AttrName
titleAttr = attrName "title"

-- draw
addCursor :: Int -> Int -> [String] -> Widget Name -> Widget Name
addCursor li ti d = showCursor (ListLocation (li, ti)) (Location (h, v))
    where v = length d - 1
          h = length $ last d

renderTask :: Int -> Int -> Task -> Widget Name
renderTask li ti t = addCursor li ti d . padBottom (Pad 1) . hLimit width . vBox $ str <$> d
    where d = wrap width $ description t

columnNumber :: Int -> String -> String
columnNumber i s = if col >= 1 && col <= 9 then show col ++ ". " ++ s else s
    where col = i + 1

renderTitle :: Int -> List -> Widget Name
renderTitle li l = withAttr titleAttr . padBottom (Pad 1) $ strWrap (columnNumber li (title l))

widget :: Int -> List -> Widget Name
widget li l = padLeftRight padding . vBox . (renderTitle li l :)  . toList $ renderTask li `mapWithIndex` tasks l

draw :: State -> [Widget Name]
draw s = [padTop (Pad 1) . hBox . toList $ widget `mapWithIndex` lists s]


-- app
chooseCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s = showCursorNamed (ListLocation (current s))

handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> halt s
        Write _ -> suspendAndResume $ do
            Persistence.Taskell.writeFile (lists s) (path s)
            return (Flow.State.continue s)
        _ -> Brick.continue s
handleEvent s _ = Brick.continue s

startEvent :: State -> EventM Name State
startEvent = return

attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr [(titleAttr, fg green)]

app :: App State e Name
app = App draw chooseCursor handleEvent startEvent attrMap'

go :: State -> IO ()
go initial = void (defaultMain app initial)
