module Flow.Keyboard where

import Graphics.Vty.Input.Events

toKey :: Event -> Maybe Key
toKey (EvKey key _) = Just key 
toKey _ = Nothing

-- return the Char pressed if there was one
getKeyChar :: Maybe Key -> Maybe Char
getKeyChar (Just (KChar char)) = Just char
getKeyChar _ = Nothing 

-- is a given Char the same as a Maybe Char
sameKeyChar :: Char -> Maybe Char -> Bool
sameKeyChar c (Just c') = c == c'
sameKeyChar c Nothing = False

-- is a given character the same as the key pressed
isChar :: Char -> Event -> Bool
isChar char = sameKeyChar char . getKeyChar . toKey

char :: Event -> Maybe Char 
char = getKeyChar . toKey

-- was the up key pressed
isUp :: Event -> Bool
isUp e = case toKey e of 
    (Just KUp) -> True
    _ -> False

-- was the down key pressed
isDown :: Event -> Bool
isDown e = case toKey e of
    (Just KDown) -> True
    _ -> False

-- was the left key pressed
isLeft :: Event -> Bool
isLeft e = case toKey e of 
    (Just KLeft) -> True
    _ -> False

-- was the right key pressed
isRight :: Event -> Bool
isRight e = case toKey e of
    (Just KRight) -> True
    _ -> False

-- was the enter key pressed
isEnter :: Event -> Bool
isEnter e = case toKey e of
    (Just KEnter) -> True
    _ -> False

isEsc :: Event -> Bool
isEsc e = case toKey e of
    (Just KEsc) -> True
    _ -> False

-- was the enter key pressed
isBS :: Event -> Bool
isBS e = case toKey e of
    (Just KBS) -> True
    _ -> False
