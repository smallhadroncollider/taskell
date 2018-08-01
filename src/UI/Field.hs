{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Field where

import ClassyPrelude

import qualified Data.Text as T (splitAt)
import qualified Data.List as L (scanl1)

import qualified Brick as B (Widget(Widget), Size(Fixed), availWidth, render, txt, vBox, Location(Location), showCursor, getContext, textWidth)
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))

import qualified UI.Types as UI (ResourceName(RNCursor))

data Field = Field {
    _text :: Text
  , _cursor :: Int
} deriving (Eq, Show)

blankField :: Field
blankField = Field "" 0

event :: V.Event -> Field -> Field
event (V.EvKey (V.KChar '\t') _) f = f
event (V.EvPaste bs) f = insertText (decodeUtf8 bs) f
event (V.EvKey V.KBS _) f = backspace f
event (V.EvKey V.KLeft _) f = updateCursor (-1) f
event (V.EvKey V.KRight _) f = updateCursor 1 f
event (V.EvKey (V.KChar char) _) f = insertCharacter char f
event _ f = f

updateCursor :: Int -> Field -> Field
updateCursor dir (Field text cursor) = Field text newCursor
    where next = cursor + dir
          limit = length text
          newCursor | next <= 0 = 0
                    | next > limit = limit
                    | otherwise = next

backspace :: Field -> Field
backspace (Field text cursor) =
    let (start, end) = T.splitAt cursor text in
    case fromNullable start of
        Nothing -> Field end cursor
        Just start' -> Field (init start' ++ end) (cursor - 1)

insertCharacter :: Char -> Field -> Field
insertCharacter char (Field text cursor) = Field newText newCursor
    where (start, end) = T.splitAt cursor text
          newText = snoc start char ++ end
          newCursor = cursor + 1

insertText :: Text -> Field -> Field
insertText insert (Field text cursor) = Field newText newCursor
    where (start, end) = T.splitAt cursor text
          newText = concat [start, insert, end]
          newCursor = cursor + length insert

insertNewLine :: Field -> Field
insertNewLine = insertText "\n"

cursorPosition :: Wrapped -> Int -> Int -> (Int, Int)
cursorPosition text width cursor =
    if x >= width then (0, y + 1) else (x, y)
    where len (t, o) = length t + o
          scanned = L.scanl1 (+) $ len <$> text
          below = takeWhile (< cursor) scanned
          x = cursor - fromMaybe 0 (lastMay below)
          y = length below

getText :: Field -> Text
getText (Field text _) = text

textToField :: Text -> Field
textToField text = Field text (length text)

field :: Field -> B.Widget UI.ResourceName
field (Field text cursor) = B.Widget B.Fixed B.Fixed $ do
    width <- B.availWidth <$> B.getContext
    let wrapped = wrap width text
        location = cursorPosition wrapped width cursor

    B.render $ if null text
        then B.showCursor UI.RNCursor (B.Location (0, 0)) $ B.txt " "
        else B.showCursor UI.RNCursor (B.Location location) . B.vBox $ B.txt . fst <$> wrapped

widgetFromMaybe :: B.Widget UI.ResourceName -> Maybe Field -> B.Widget UI.ResourceName
widgetFromMaybe _ (Just f) = field f
widgetFromMaybe w Nothing = w

textField :: Text -> B.Widget UI.ResourceName
textField text = B.Widget B.Fixed B.Fixed $ do
    width <- B.availWidth <$> B.getContext
    let wrapped = wrap width text
    B.render $ if null text
        then B.txt "---"
        else B.vBox $ B.txt . fst <$> wrapped

-- text wrapping
type Wrapped = [(Text, Int)]

wrap :: Int -> Text -> Wrapped
wrap width = foldl' (combine width) [] . spl

spl' :: [Text] -> Char -> [Text]
spl' ts c
    | c == ' ' = ts ++ [" "] ++ [""]
    | c == '\n' = ts ++ ["\n"] ++ [""]
    | otherwise = case fromNullable ts of
        Just ts' -> init ts' ++ [snoc (last ts') c]
        Nothing -> [singleton c]

spl :: Text -> [Text]
spl = foldl' spl' [""]

combine :: Int -> Wrapped -> Text -> Wrapped
combine width acc s
    | s == "\n" = accInit ++ [(text, offset + 1)] ++ [("", 0)]
    | newline && s == " " = acc ++ [("", 1)]
    | text == "" && s == " " = accInit ++ [("", offset + 1)]
    | newline = acc ++ [(s, 0)]
    | otherwise = accInit ++ [(text ++ s, offset)]
    where (text, offset) = fromMaybe ("", 0) (lastMay acc)
          accInit = fromMaybe [] (initMay acc)
          newline = B.textWidth text + B.textWidth s > width
