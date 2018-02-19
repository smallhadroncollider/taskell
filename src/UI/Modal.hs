{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal (
    showModal
) where

import Events.State (State, Mode(..), ModalType(..), mode, getCurrentTask)
import Events.State.Modal.SubTasks (getCurrentSubTask)
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.Taskell.Task (SubTask, description, subTasks, name, complete)
import Data.Text as T (Text, lines, replace, breakOn, strip, drop, length, append)
import Data.Text.Encoding (decodeUtf8)
import Data.Taskell.Text (wrap)
import Data.FileEmbed (embedFile)
import Data.Foldable (toList)
import Data.Sequence (mapWithIndex)
import Data.Maybe (fromMaybe)
import IO.Markdown (trimListItem)

import UI.Types (ResourceName(..))
import UI.Theme (titleAttr, taskCurrentAttr, disabledAttr)

maxWidth :: Int
maxWidth = 50

modal :: Int -> (Text, Widget ResourceName) -> Widget ResourceName
modal width (title, w) = t <=> w'
    where t = padBottom (Pad 1) . withAttr titleAttr $ box (wrap width title)
          w'= viewport RNModal Vertical w

surround :: (Int -> (Text, Widget ResourceName)) -> Widget ResourceName
surround fn = Widget Fixed Fixed $ do
    ctx <- getContext
    let fullWidth = availWidth ctx
        padding = 4
        w = (if fullWidth > maxWidth then maxWidth else fullWidth) - (padding * 2)
        widget = modal w $ fn w
    render . padTopBottom 1 . centerLayer . border . padTopBottom 1 . padLeftRight padding . hLimit w $ widget


help :: Int -> (Text, Widget ResourceName)
help _ = ("Controls", w)
    where ls = T.lines $ decodeUtf8 $(embedFile "templates/controls.md")
          (l, r) = unzip $ breakOn ":" . T.replace "`" "" . trimListItem <$> ls
          left = padLeftRight 2 . vBox $ withAttr taskCurrentAttr . txt <$> l
          right = vBox $ txt . T.strip . T.drop 1 <$> r
          w = left <+> right

addCursor :: Int -> [Text] -> Widget ResourceName -> Widget ResourceName
addCursor li d = showCursor n (Location (h, v))

    where v = Prelude.length d - 1
          h = T.length $ last d
          n = RNModalItem li

box :: [Text] -> Widget ResourceName
box d = padBottom (Pad 1) . vBox $ txt <$> d

renderSubTask :: Int -> Int -> Int -> SubTask -> Widget ResourceName
renderSubTask width current i subtask | i == current = visible $ withAttr taskCurrentAttr widget
                                | complete subtask = withAttr disabledAttr widget
                                | otherwise = widget
    where postfix = if complete subtask then " ✓" else ""
          text = wrap width $ name subtask `T.append` postfix
          widget = addCursor i text (box text)

st' :: State -> Int -> Maybe (Text, Widget ResourceName)
st' state width = do
    task <- getCurrentTask state
    index <- getCurrentSubTask state
    let sts = subTasks task
        w | null sts = withAttr disabledAttr $ txt "  No sub-tasks"
          | otherwise = vBox . toList $ renderSubTask width index `mapWithIndex` sts
    return (description task, w)

st :: State -> Int -> (Text, Widget ResourceName)
st state width = fromMaybe ("Error", txt "Oops") $ st' state width

getModal :: State -> ModalType -> [Widget ResourceName]
getModal s t = case t of
    Help -> [surround help]
    SubTasks _ _ -> [surround $ st s]

showModal :: State -> [Widget ResourceName] -> [Widget ResourceName]
showModal s view = case mode s of
    Modal t -> getModal s t ++ view
    _ -> view
