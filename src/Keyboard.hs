module Keyboard (
    isChar,
    isUp,
    isDown
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
isChar char (EvKey key _) = sameKeyChar char $ getKeyChar key
isChar _ _ = False

isUp :: Event -> Bool
isUp (EvKey key _) = isUp' key
    where
        isUp' :: Key -> Bool
        isUp' KUp = True
        isUp' _ = False
isUp _ = False

isDown :: Event -> Bool
isDown (EvKey key _) = isDown' key
    where
        isDown' :: Key -> Bool
        isDown' KDown = True
        isDown' _ = False
isDown _ = False
