module TaskellJSON where

import System.Directory
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS

import CLI (promptYN)
import Task (Tasks, empty)

path :: FilePath
path = "taskell.json"

exists :: IO Bool
exists = do
    go <- doesFileExist path >>= promptCreate
    if go then return True else return False

-- prompt whether to create taskell.json
promptCreate :: Bool -> IO Bool
promptCreate True = return True
promptCreate False = do
    cwd <- getCurrentDirectory
    r <- promptYN $ "Create " ++ cwd ++ "/" ++ path ++ "?"
    if r then (createPath >> return True) else return False

-- creates taskell file
createPath :: IO ()
createPath = writeFile path "[]"

-- writes Tasks to json file
writeJSON :: Tasks -> IO ()
writeJSON tasks = BS.writeFile "taskell.json" $ encodePretty tasks

-- reads json file
readJSON :: IO Tasks
readJSON = do
    x <- BS.readFile path 
    return $ jsonToTasks x

-- return tasks or empty list
deMaybe :: Maybe Tasks -> Tasks
deMaybe (Just ts) = ts
deMaybe Nothing = empty 

-- returns a Maybe
jsonToTasks' :: BS.ByteString -> Maybe Tasks
jsonToTasks' s = decode s :: Maybe Tasks

-- returns tasks or an empty list
jsonToTasks :: BS.ByteString -> Tasks
jsonToTasks = deMaybe . jsonToTasks'
