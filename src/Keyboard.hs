module Keyboard (
    isChar
) where

import Graphics.Vty.Input.Events

getKeyChar :: Key -> Maybe Char
getKeyChar (KChar char) = Just char
getKeyChar _ = Nothing 

sameKeyChar :: Char -> Maybe Char -> Bool
sameKeyChar c (Just c') = c == c'
sameKeyChar c Nothing = False

isChar :: Char -> Event -> Bool
isChar char (EvKey key modifer) = sameKeyChar char $ getKeyChar key
isChar _ _ = False
