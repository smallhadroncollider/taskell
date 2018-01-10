module Persistence.Taskell where

import System.Directory
import System.Environment (getArgs)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)
import Persistence.Markdown (stringify, parse)
import qualified Data.ByteString.Lazy as BS

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
createPath = writeJSON initial

-- writes Tasks to json file
writeJSON :: Lists -> FilePath -> IO ()
writeJSON tasks path | ".json" `isSuffixOf` path = BS.writeFile path $ encodePretty tasks
                     | otherwise = BS.writeFile path $ stringify tasks

-- reads json file
readJSON :: FilePath -> IO Lists
readJSON path = do
    content <- BS.readFile path
    let ls | ".json" `isSuffixOf` path = jsonToTasks content
           | otherwise = parse content
    return ls

-- returns tasks or an empty list
jsonToTasks :: BS.ByteString -> Lists
jsonToTasks s = fromMaybe initial (decode s :: Maybe Lists)
