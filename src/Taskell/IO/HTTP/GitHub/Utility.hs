module Taskell.IO.HTTP.GitHub.Utility
    ( cleanUp
    ) where

import ClassyPrelude

import Data.Text (replace)

cleanUp :: Text -> Text
cleanUp txt = replace "\r" "" $ replace "\n" " " txt
