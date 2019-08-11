{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Field where

import ClassyPrelude

import qualified Data.List as L (scanl1)
import qualified Data.Text as T (splitAt, takeEnd)

import qualified Brick                     as B (Location (Location), Size (Fixed), Widget (Widget),
                                                 availWidth, getContext, render, showCursor, txt,
                                                 vBox)
import qualified Brick.Widgets.Core        as B (textWidth)
import qualified Graphics.Vty.Input.Events as V (Event (..), Key (..))

import qualified UI.Types as UI (ResourceName (RNCursor))

data Field = Field
    { _text   :: Text
    , _cursor :: Int
    } deriving (Eq, Show)

blankField :: Field
blankField = Field "" 0

event :: V.Event -> Field -> Field
event (V.EvKey (V.KChar '\t') _) f = f
event (V.EvPaste bs) f             = insertText (decodeUtf8 bs) f
event (V.EvKey V.KBS _) f          = backspace f
event (V.EvKey V.KLeft _) f        = updateCursor (-1) f
event (V.EvKey V.KRight _) f       = updateCursor 1 f
event (V.EvKey (V.KChar char) _) f = insertCharacter char f
event _ f                          = f

updateCursor :: Int -> Field -> Field
updateCursor dir (Field text cursor) = Field text newCursor
  where
    next = cursor + dir
    limit = length text
    newCursor
        | next <= 0 = 0
        | next > limit = limit
        | otherwise = next

backspace :: Field -> Field
backspace (Field text cursor) =
    let (start, end) = T.splitAt cursor text
    in case fromNullable start of
           Nothing     -> Field end cursor
           Just start' -> Field (init start' <> end) (cursor - 1)

insertCharacter :: Char -> Field -> Field
insertCharacter char (Field text cursor) = Field newText newCursor
  where
    (start, end) = T.splitAt cursor text
    newText = snoc start char <> end
    newCursor = cursor + 1

insertText :: Text -> Field -> Field
insertText insert (Field text cursor) = Field newText newCursor
  where
    (start, end) = T.splitAt cursor text
    newText = concat [start, insert, end]
    newCursor = cursor + length insert

cursorPosition :: [Text] -> Int -> Int -> (Int, Int)
cursorPosition text width cursor =
    if x == width
        then (0, y + 1)
        else (x, y)
  where
    scanned = L.scanl1 (+) $ length <$> text
    below = takeWhile (< cursor) scanned
    x = cursor - maybe 0 last (fromNullable below)
    y = length below

getText :: Field -> Text
getText (Field text _) = text

textToField :: Text -> Field
textToField text = Field text (length text)

field :: Field -> B.Widget UI.ResourceName
field (Field text cursor) =
    B.Widget B.Fixed B.Fixed $ do
        width <- B.availWidth <$> B.getContext
        let (wrapped, offset) = wrap width text
            location = cursorPosition wrapped width (cursor - offset)
        B.render $
            if null text
                then B.showCursor UI.RNCursor (B.Location (0, 0)) $ B.txt " "
                else B.showCursor UI.RNCursor (B.Location location) . B.vBox $ B.txt <$> wrapped

widgetFromMaybe :: B.Widget UI.ResourceName -> Maybe Field -> B.Widget UI.ResourceName
widgetFromMaybe _ (Just f) = field f
widgetFromMaybe w Nothing  = w

textField :: Text -> B.Widget UI.ResourceName
textField text =
    B.Widget B.Fixed B.Fixed $ do
        width <- B.availWidth <$> B.getContext
        let (wrapped, _) = wrap width text
        B.render $
            if null text
                then B.txt "---"
                else B.vBox $ B.txt <$> wrapped

-- wrap
wrap :: Int -> Text -> ([Text], Int)
wrap width = foldl' (combine width) ([""], 0) . spl

spl' :: [Text] -> Char -> [Text]
spl' ts c
    | c == ' ' = ts <> [" "] <> [""]
    | otherwise =
        case fromNullable ts of
            Just ts' -> init ts' <> [snoc (last ts') c]
            Nothing  -> [singleton c]

spl :: Text -> [Text]
spl = foldl' spl' [""]

combine :: Int -> ([Text], Int) -> Text -> ([Text], Int)
combine width (acc, offset) s
    | newline && s == " " = (acc, offset + 1)
    | T.takeEnd 1 l == " " && s == " " = (acc, offset + 1)
    | newline = (acc <> [s], offset)
    | otherwise = (append (l <> s) acc, offset)
  where
    l = maybe "" last (fromNullable acc)
    newline = B.textWidth l + B.textWidth s > width

append :: Text -> [Text] -> [Text]
append s l =
    case fromNullable l of
        Just l' -> init l' <> [s]
        Nothing -> l <> [s]
