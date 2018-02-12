module IO.Taskell where

import Prelude hiding (writeFile)
import System.Directory
import System.Environment (getArgs)
import Control.Monad (void)
import IO.Markdown (stringify, parse)
import IO.Config (Config, general, filename)
import qualified Data.ByteString as BS

import UI.CLI (promptYN)
import Data.Taskell.Lists (Lists, initial)

getPath :: Config -> IO String
getPath c = do
    let defaultPath = filename $ general c
    args <- getArgs
    return $ if not (null args) then head args else defaultPath

exists :: Config -> IO (Bool, FilePath)
exists c = do
    path <- getPath c
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
writeFile tasks path = void (BS.writeFile path $ stringify tasks)

-- reads json file
readFile :: FilePath -> IO Lists
readFile path = do
    content <- BS.readFile path
    return $ parse content
