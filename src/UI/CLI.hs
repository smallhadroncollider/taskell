module UI.CLI where

import System.IO (stdout, hFlush)

prompt :: String -> IO String
prompt s = do
    putStr $ s ++ ": "
    hFlush stdout -- prevents buffering 
    getLine

promptYN :: String -> IO Bool
promptYN s = (==) "y" <$> prompt (s ++ " (y/n)")
