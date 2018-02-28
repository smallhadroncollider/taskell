{-# LANGUAGE OverloadedStrings #-}
module UI.Field where

import qualified Brick as B (Widget(Widget), Size(Fixed), availWidth, render, txt, vBox, Location(Location), showCursor, getContext)
import qualified Data.Taskell.Text as T (wrap)
import qualified Data.Text as T (Text, length, snoc, init, append, null, splitAt, concat)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Graphics.Vty.Input.Events as V (Event(..), Key(..))
import qualified UI.Types as UI (ResourceName)

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

cursorPosition :: [T.Text] -> Int -> (Int, Int)
cursorPosition text cursor =
    if null below
        then (cursor, 0)
        else (cursor - last below, length below - 1)
    where scanned = scanl (+) 0 $ T.length <$> text
          below = takeWhile (<= cursor) scanned

getText :: Field -> T.Text
getText (Field text _) = text

textToField :: T.Text -> Field
textToField text = Field text (T.length text)

field :: UI.ResourceName -> Field -> B.Widget UI.ResourceName
field name (Field text cursor) = B.Widget B.Fixed B.Fixed $ do
    width <- B.availWidth <$> B.getContext
    let wrapped = T.wrap width $ text `T.append` " "
        location = cursorPosition wrapped cursor
    B.render . B.showCursor name (B.Location location) . B.vBox $ B.txt <$> wrapped

textField :: T.Text -> B.Widget UI.ResourceName
textField text = B.Widget B.Fixed B.Fixed $ do
    width <- B.availWidth <$> B.getContext
    B.render . B.vBox $ B.txt <$> T.wrap width text
