{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.CLI
    ( prompt
    , promptYN
    , PromptYN(PromptYes)
    ) where

import ClassyPrelude

prompt :: Text -> IO Text
prompt s = do
    putStr $ s <> ": "
    hFlush stdout -- prevents buffering
    getLine

data PromptYN
    = PromptYes
    | PromptNo

promptYN :: PromptYN -> Text -> IO Bool
promptYN PromptYes s = not . flip elem ["n", "no"] . toLower <$> prompt (s <> " (Y/n)")
promptYN PromptNo s  = not . flip elem ["y", "yes"] . toLower <$> prompt (s <> " (y/N)")
