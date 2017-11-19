module UI.Render (
    render
) where

import Prelude hiding (filter)
import Data.Sequence (mapWithIndex, filter) 
import Data.Foldable (toList)
import Graphics.Vty

import Flow.State
import Flow.Actions
import Data.Taskell.Task (Task, Tasks, description, completed, filterCompleted)
import Persistence.Taskell (writeJSON)
import UI.Task (present)
import UI.Main (title)

-- filter out completed if option set
getTasks :: State -> Tasks
getTasks s = if showCompleted s then ts else filterCompleted ts
    where ts = tasks s

-- draws the screen
pic :: State -> Picture
pic state = picForImage $ title <-> imgs
    where
        bullet' = present $ current state
        imgs = vertCat $ toList $ mapWithIndex bullet' $ getTasks state

-- the draw loop
draw :: Vty -> State -> IO ()
draw vty state = do
    update vty $ pic state
    e <- nextEvent vty
    
    -- get updated version of state
    let state' = event e state

    -- update json if tasks changed
    if tasks state == tasks state'
        then return ()
        else writeJSON $ tasks state'
    
    -- if event wasn't quit keep going, otherwise shutdown
    if running state'
        then draw vty state'
        else shutdown vty

-- setup vty and start the draw loop
render :: State -> IO ()
render state = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    draw vty state
