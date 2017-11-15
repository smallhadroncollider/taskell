module Lib (
    render
) where

import Graphics.Vty

draw :: Vty -> IO ()
draw vty = do
     let title = string (defAttr ` withForeColor ` green) ("Taskell")
         img = title
         pic = picForImage img
     update vty pic

     e <- nextEvent vty

     if (show e) == "EvKey (KChar 'q') []"
         then shutdown vty
         else draw vty

render :: IO ()
render = do
     cfg <- standardIOConfig
     vty <- mkVty cfg
     draw vty
