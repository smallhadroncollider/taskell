module Lib (
    render
) where

import Graphics.Vty
import Keyboard

pic :: Picture
pic = picForImage img
    where
        title = string (defAttr ` withForeColor ` green) ("[Taskell]")
        img = title

next :: Event -> (Vty -> IO ())
next e = if isChar 'q' e then shutdown else draw

draw :: Vty -> IO ()
draw vty = do
     update vty pic
     e <- nextEvent vty
     next e vty

render :: IO ()
render = do
     cfg <- standardIOConfig
     vty <- mkVty cfg
     draw vty
