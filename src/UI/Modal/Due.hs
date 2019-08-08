{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal.Due
    ( due
    ) where

import ClassyPrelude

import Brick
import Control.Lens ((^.))

import           Data.Taskell.Date  (Day, dayToText, deadline)
import qualified Data.Taskell.Lists as L (due)
import qualified Data.Taskell.Task  as T (Task, due, name)
import           Events.State.Types (State, lists)
import           UI.Theme           (dlToAttr)
import           UI.Types           (ResourceName)

renderDate :: Day -> Maybe Day -> Maybe (Widget ResourceName)
renderDate today dueDay = do
    let attr = withAttr . dlToAttr . deadline today <$> dueDay
        widget = txt . dayToText today <$> dueDay
    attr <*> widget

renderTask :: Day -> T.Task -> Widget ResourceName
renderTask today task = du <+> name
  where
    name = padLeftRight 1 . txt $ task ^. T.name
    du = fromMaybe emptyWidget (renderDate today (task ^. T.due))

due :: State -> Day -> (Text, Widget ResourceName)
due state today = ("Due Tasks", widget)
  where
    tasks = L.due $ state ^. lists
    widget = vBox . toList $ renderTask today <$> tasks
