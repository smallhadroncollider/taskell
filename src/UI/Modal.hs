{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal (
    showModal
) where

import Events.State (State, Mode(..), ModalType(..), mode, getCurrentTask)
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.Taskell.Task (description, subTasks, name, complete)
import Data.Text as T (Text, lines, replace, breakOn, strip, drop, append)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)
import IO.Markdown (trimListItem)

import UI.Types (ResourceName(..))
import UI.Theme (titleAttr, taskCurrentAttr)

place :: Widget ResourceName -> Widget ResourceName
place = centerLayer . border . padTopBottom 1 . padLeftRight 4

modal :: Text -> Widget ResourceName -> Widget ResourceName
modal title w = place $ t <=> w
    where t = padBottom (Pad 1) . withAttr titleAttr $ txt title

help :: Widget ResourceName
help = modal "Controls" w
    where ls = T.lines $ decodeUtf8 $(embedFile "templates/controls.md")
          (l, r) = unzip $ breakOn ":" . T.replace "`" "" . trimListItem <$> ls
          left = padRight (Pad 2) . vBox $ withAttr taskCurrentAttr . txt <$> l
          right = vBox $ txt . T.strip . T.drop 1 <$> r
          w = left <+> right

st :: State -> Maybe (Widget ResourceName)
st s = do
    t <- getCurrentTask s
    let sts = subTasks t
    return $ modal (description t) $ vBox $ txt . rndr <$> sts
    where rndr t = name t `append` (if complete t then " ✓" else "")

getModal :: State -> ModalType -> [Widget ResourceName]
getModal s t = case t of
    Help -> [help]
    SubTasks -> case st s of
        Just w -> [w]
        Nothing -> []

showModal :: State -> [Widget ResourceName] -> [Widget ResourceName]
showModal s view = case mode s of
    Modal t -> getModal s t ++ view
    _ -> view
