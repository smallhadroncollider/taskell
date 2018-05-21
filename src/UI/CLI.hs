{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.CLI (
    promptYN
) where

import ClassyPrelude

prompt :: Text -> IO Text
prompt s = do
    putStr $ s ++ ": "
    hFlush stdout -- prevents buffering
    getLine

promptYN :: Text -> IO Bool
promptYN s = (==) "y" <$> prompt (s ++ " (y/n)")
