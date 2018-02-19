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
    success <- promptCreate c e path
    return (success, path)

-- prompt whether to create taskell.json
promptCreate :: Config -> Bool -> String -> IO Bool
promptCreate _ True _ = return True
promptCreate config False path = do
    cwd <- getCurrentDirectory
    r <- promptYN $ "Create " ++ cwd ++ "/" ++ path ++ "?"
    if r then createPath config path >> return True else return False

-- creates taskell file
createPath :: Config -> FilePath -> IO ()
createPath config = writeFile config initial

-- writes Tasks to json file
writeFile :: Config -> Lists -> FilePath -> IO ()
writeFile config tasks path = void (BS.writeFile path $ stringify config tasks)

-- reads json file
readFile :: Config -> FilePath -> IO (Either String Lists)
readFile config path = do
    content <- BS.readFile path
    return $ parse config content
