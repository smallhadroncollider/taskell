{-# LANGUAGE OverloadedStrings #-}
module UI.Field where

import qualified Brick as B (Widget(Widget), Size(Fixed), availWidth, render, txt, vBox, Location(Location), showCursor, getContext)
import qualified Brick.Widgets.Core as B (textWidth)
import qualified Data.List as L (foldl')
import qualified Data.Text as T (Text, length, snoc, init, append, null, splitAt, concat, singleton, foldl', last)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import qualified Safe as S (lastDef, scanl1Def)
import qualified UI.Types as UI (ResourceName(RNCursor))

data Field = Field {
    _text :: T.Text
  , _cursor :: Int
} deriving (Eq, Show)

blankField :: Field
blankField = Field "" 0

event :: V.Event -> Field -> Field
event (V.EvKey (V.KChar '\t') _) f = f
event (V.EvPaste bs) f = insertText (T.decodeUtf8 bs) f
event (V.EvKey V.KBS _) f = backspace f
event (V.EvKey V.KLeft _) f = updateCursor (-1) f
event (V.EvKey V.KRight _) f = updateCursor 1 f
event (V.EvKey (V.KChar char) _) f = insertCharacter char f
event _ f = f

updateCursor :: Int -> Field -> Field
updateCursor dir (Field text cursor) = Field text newCursor
    where next = cursor + dir
          limit = T.length text
          newCursor | next <= 0 = 0
                    | next > limit = limit
                    | otherwise = next

backspace :: Field -> Field
backspace (Field text cursor) =
    let (start, end) = T.splitAt cursor text in
    if T.null start
        then Field end cursor
        else Field (T.init start `T.append` end) (cursor - 1)

insertCharacter :: Char -> Field -> Field
insertCharacter char (Field text cursor) = Field newText newCursor
    where (start, end) = T.splitAt cursor text
          newText = T.snoc start char `T.append` end
          newCursor = cursor + 1

insertText :: T.Text -> Field -> Field
insertText insert (Field text cursor) = Field newText newCursor
    where (start, end) = T.splitAt cursor text
          newText = T.concat [start, insert, end]
          newCursor = cursor + T.length insert

cursorPosition :: [T.Text] -> Int -> Int -> (Int, Int)
cursorPosition text width cursor =
    if x == width then (0, y + 1) else (x, y)
    where scanned = S.scanl1Def [] (+) $ T.length <$> text
          below = takeWhile (< cursor) scanned
          x = cursor - S.lastDef 0 below
          y = length below

getText :: Field -> T.Text
getText (Field text _) = text

textToField :: T.Text -> Field
textToField text = Field text (T.length text)

field :: Field -> B.Widget UI.ResourceName
field (Field text cursor) = B.Widget B.Fixed B.Fixed $ do
    width <- B.availWidth <$> B.getContext
    let (wrapped, offset) = wrap width text
        location = cursorPosition wrapped width (cursor - offset)

    B.render $ if T.null text
        then B.showCursor UI.RNCursor (B.Location (0, 0)) $ B.txt " "
        else B.showCursor UI.RNCursor (B.Location location) . B.vBox $ B.txt <$> wrapped

fromMaybe :: B.Widget UI.ResourceName -> Maybe Field -> B.Widget UI.ResourceName
fromMaybe _ (Just f) = field f
fromMaybe w Nothing = w

textField :: T.Text -> B.Widget UI.ResourceName
textField text = B.Widget B.Fixed B.Fixed $ do
    width <- B.availWidth <$> B.getContext
    let (wrapped, _) = wrap width text
    B.render $ if T.null text
        then B.txt " "
        else B.vBox $ B.txt <$> wrapped

-- wrap
wrap :: Int -> T.Text -> ([T.Text], Int)
wrap width = L.foldl' (combine width) ([""], 0) . spl

spl' :: [T.Text] -> Char -> [T.Text]
spl' ts c
    | c == ' ' = ts ++ [" "] ++ [""]
    | Prelude.null ts = [T.singleton c]
    | otherwise = Prelude.init ts ++ [T.snoc (Prelude.last ts) c]

spl :: T.Text -> [T.Text]
spl = T.foldl' spl' [""]

combine :: Int -> ([T.Text], Int) -> T.Text -> ([T.Text], Int)
combine width (acc, offset) s
    | newline && s == " " = (acc, offset + 1)
    | not (T.null l) && T.last l == ' ' && s == " " = (acc, offset + 1)
    | newline = (acc ++ [s], offset)
    | otherwise = (append (l `T.append` s) acc, offset)
    where l = if Prelude.null acc then "" else last acc
          newline = B.textWidth l + B.textWidth s > width

append :: T.Text -> [T.Text] -> [T.Text]
append s l = l' ++ [s]
    where l' = if Prelude.null l then l else Prelude.init l
