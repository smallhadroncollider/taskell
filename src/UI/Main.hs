module UI.Main where

import Graphics.Vty
import Data.Foldable (toList)

import Data.Sequence (Seq, (<|))

import Flow.State (State, lists)

import UI.List (ListUI(ListUI), present)
import UI.Task (TaskUI(TaskUI))
import UI.Styles

renderTask :: TaskUI -> Image
renderTask (TaskUI lines _) = vertCat $ map (string currentAttr) lines 

renderList :: ListUI -> Seq Image
renderList (ListUI title ts) = string attrTitle title <| (renderTask <$> ts)

hCat :: Seq Image -> Image
hCat = foldl (<|>) emptyImage

vCat :: Seq Image -> Image
vCat = foldl (<->) emptyImage

-- draws the screen
pic :: State -> Picture
pic s = Picture (Cursor 0 0) [image] ClearBackground
    where ts = present <$> lists s
          image = hCat $ (margin . vCat . fmap marginTop . renderList) <$> ts
