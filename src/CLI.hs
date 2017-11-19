module CLI where

import System.IO (stdout, hFlush)

prompt :: String -> IO String
prompt s = do
    putStr $ s ++ ": "
    hFlush stdout -- prevents buffering 
    getLine >>= return

promptYN :: String -> IO Bool
promptYN s = (prompt $ s ++ " (y/n)") >>= return . (==) "y"
