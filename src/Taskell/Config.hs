{-# LANGUAGE TemplateHaskell #-}

module Taskell.Config where

import ClassyPrelude

import           Data.FileEmbed             (embedFile)
import           Data.Version               (showVersion)
import           Language.Haskell.TH.Syntax (liftString)
import qualified Paths_taskell              (version)

version :: Text
version = $(liftString $ showVersion Paths_taskell.version)

trelloUsage :: Text
trelloUsage = decodeUtf8 $(embedFile "templates/trello-token.txt")

githubUsage :: Text
githubUsage = decodeUtf8 $(embedFile "templates/github-token.txt")
