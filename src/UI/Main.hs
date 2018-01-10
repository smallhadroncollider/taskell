module UI.Main where

import Graphics.Vty hiding (showCursor)
import Data.Maybe (fromMaybe)

import Data.List (foldl')
import Data.Sequence (Seq, mapWithIndex)

import Flow.State (State, Pointer, Size, Mode(..), InsertMode(..), mode, lists, current, size, newList, search)

import UI.Styles

import Config (width, padding)
import Data.Taskell.String (wrap)
import Data.Taskell.Seq (Split, splitOn)
import Data.Taskell.Task (description)
import qualified Data.Taskell.List as List (List, tasks, title)

type TaskUI = [String]
type ListUI = (TaskUI, Seq TaskUI)

columnNumber :: Int -> String -> String
columnNumber i s = if col >= 1 && col <= 9 then show col ++ ". " ++ s else s
    where col = i + 1

present :: Int -> List.List -> ListUI
present i l = (wrap width (columnNumber i $ List.title l), wrap width . description <$> List.tasks l)

currentTitleImage :: TaskUI -> Image
currentTitleImage = img attrCurrentTitle

taskLength:: TaskUI -> Int
taskLength = sum . fmap length

tasksImage :: Seq TaskUI -> Image
tasksImage = vCat . fmap (marginTop . taskImage)

renderCurrentList' :: Size -> TaskUI -> Split TaskUI -> Bool -> (Image, Int, Int)
renderCurrentList' (_, height) listTitle (before, cur, after) tc = (translateY yOffset image, x, y + yOffset)
    where title = currentTitleImage listTitle
          [before', after'] = tasksImage <$> [before, after]
          cur' = marginTop (currentTaskImage cur)

          y | tc = imageHeight title
            | otherwise = imageHeight title + sum (imageHeight <$> [before', cur'])

          x | tc = length (last listTitle)
            | not (null cur) = length (last cur)
            | otherwise = 0

          yOffset = calcOffset (height `div` 2) y
          image = margin $ vertCat [title, before', cur', after']

renderCurrentList :: Size -> Int -> ListUI -> Bool -> (Image, Int, Int)
renderCurrentList sz index (title, tasks) tc = case splitOn index tasks of
    Just list -> renderCurrentList' sz title list tc
    Nothing -> let t = margin (currentTitleImage title) in (t, length (last title), imageHeight t)

listImage :: ListUI -> Image
listImage (title, tasks) = margin $ img attrTitle title <-> tasksImage tasks

listsImage :: Seq ListUI -> Image
listsImage = hCat . fmap listImage

renderLists' :: Pointer -> Size -> Seq ListUI -> Bool -> Maybe (Image, Int, Int, Int)
renderLists' (list, index) sz ls tc = do
    (before, cur, after) <- splitOn list ls
    let [before', after'] = listsImage <$> [before, after]
    let (current', x, y) = renderCurrentList sz index cur tc
    let image = horizCat [before', current', after']
    return (image, imageWidth before', x, y)

renderLists :: Pointer -> Size -> Seq ListUI -> Bool -> (Image, Int, Int, Int)
renderLists p s ls tc = fromMaybe (string attrNormal "No lists", 0, 0, 0) c
    where c = renderLists' p s ls tc

calcOffset :: Int -> Int -> Int
calcOffset pivot n = if n > pivot then pivot - n else 0

-- draws the screen
pic :: State -> Picture
pic state = Picture cursor [searchImage state (snd sz) image'] ClearBackground
    where state' = search $ newList state
          sz = size state'
          ls = mapWithIndex present $ lists state'
          (image, w, x, y) = renderLists (current state') sz ls (titleCursor state')
          o = calcOffset (fst sz `div` 3) w
          image' = translateX o $ marginTop image
          cursor = if showCursor state' then Cursor (w + x + o + padding) y else NoCursor

showCursor :: State -> Bool
showCursor s = case mode s of
    Insert _ -> True
    _ -> False

titleCursor :: State -> Bool
titleCursor s = case mode s of
    Insert EditList -> True
    _ -> False

searchImage :: State -> Int -> Image -> Image
searchImage s h i = case mode s of
    Search ent term ->
        let style = if ent then attrCurrent else attrNormal
            offset = imageHeight i + 3
            top = if h > offset then h - offset else 0
            p = pad padding top 0 padding
        in
            marginBottom (cropBottom (h - 3) i) <-> p (string style ("/" ++ term))
    _ -> i

-- styling
taskImage :: TaskUI -> Image
taskImage = img attrNormal

currentTaskImage :: TaskUI -> Image
currentTaskImage = img attrCurrent

-- vty helpers
img :: Attr -> TaskUI -> Image
img a s = vertCat $ string a <$> s

hCat :: Seq Image -> Image
hCat = foldl' (<|>) emptyImage

vCat :: Seq Image -> Image
vCat = foldl' (<->) emptyImage
