module CLI where

import System.IO (stdout, hFlush)

prompt :: String -> IO String
prompt s = do
    putStr $ s ++ ": "
    hFlush stdout -- prevents buffering 
    r <- getLine
    return r

promptYN :: String -> IO Bool
promptYN s = do
    r <- prompt $ s ++ " (y/n)"
    if r == "y" then return True else return False
