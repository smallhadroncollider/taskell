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
import           Events.State.Types (lists)
import           UI.Draw.Types      (DrawState (..), ReaderDrawState)
import           UI.Theme           (dlToAttr)
import           UI.Types           (ResourceName)

renderDate :: Maybe Day -> ReaderDrawState (Maybe (Widget ResourceName))
renderDate dueDay = do
    today <- asks dsToday
    let attr = withAttr . dlToAttr . deadline today <$> dueDay
        widget = txt . dayToText today <$> dueDay
    pure $ attr <*> widget

renderTask :: T.Task -> ReaderDrawState (Widget ResourceName)
renderTask task = do
    date <- renderDate (task ^. T.due)
    let name = padLeftRight 1 . txt $ task ^. T.name
    let du = fromMaybe emptyWidget date
    pure $ du <+> name

due :: ReaderDrawState (Text, Widget ResourceName)
due = do
    tasks <- L.due . (^. lists) <$> asks dsState
    widgets <- sequence $ renderTask <$> tasks
    pure ("Due Tasks", vBox $ toList widgets)
