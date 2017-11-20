module Render (
    render
) where

import Graphics.Vty (Vty, standardIOConfig, mkVty, nextEvent, update, shutdown)

import Flow.State (State, tasks, getTasks, running)
import Flow.Actions (event)
import Persistence.Taskell (writeJSON)
import UI.Main (pic)

-- the draw loop
loop :: Vty -> State -> IO ()
loop vty state = do
    update vty $ pic state
    e <- nextEvent vty
    
    -- get updated version of state
    let state' = event e state

    -- update json if tasks changed
    if tasks state == tasks state'
        then return ()
        else writeJSON $ getTasks state'
    
    -- if event wasn't quit keep going, otherwise shutdown
    if running state'
        then loop vty state'
        else shutdown vty

-- setup vty and start the loop
render :: State -> IO ()
render state = do
    vty <- standardIOConfig >>= mkVty
    loop vty state
