module Persistence.Taskell where

import Prelude hiding (writeFile, readFile)
import System.Directory
import System.Environment (getArgs)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Persistence.Markdown (stringify, parse)
import qualified Data.ByteString as BS

import UI.CLI (promptYN)
import Data.Taskell.Lists (Lists, initial)

import Config (defaultPath)

getPath :: IO String
getPath = do
    args <- getArgs
    return $ if not (null args) then head args else defaultPath

exists :: IO (Bool, FilePath)
exists = do
    path <- getPath
    e <- doesFileExist path
    success <- promptCreate e path
    return (success, path)

-- prompt whether to create taskell.json
promptCreate :: Bool -> String -> IO Bool
promptCreate True _ = return True
promptCreate False path = do
    cwd <- getCurrentDirectory
    r <- promptYN $ "Create " ++ cwd ++ "/" ++ path ++ "?"
    if r then createPath path >> return True else return False

-- creates taskell file
createPath :: FilePath -> IO ()
createPath = writeFile initial

-- writes Tasks to json file
writeFile :: Lists -> FilePath -> IO ()
writeFile tasks path = void (forkIO . BS.writeFile path $ stringify tasks)

-- reads json file
readFile :: FilePath -> IO Lists
readFile path = do
    content <- BS.readFile path
    return $ parse content
