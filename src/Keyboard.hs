module Keyboard (
    isChar
) where

import Graphics.Vty.Input.Events

-- return the Char pressed if there was one
getKeyChar :: Key -> Maybe Char
getKeyChar (KChar char) = Just char
getKeyChar _ = Nothing 

-- is a given Char the same as a Maybe Char
sameKeyChar :: Char -> Maybe Char -> Bool
sameKeyChar c (Just c') = c == c'
sameKeyChar c Nothing = False

-- is a given character the same as the key pressed
isChar :: Char -> Event -> Bool
isChar char (EvKey key modifer) = sameKeyChar char $ getKeyChar key
isChar _ _ = False
