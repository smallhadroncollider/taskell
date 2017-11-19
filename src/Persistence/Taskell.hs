module Persistence.Taskell where

import System.Directory
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS

import UI.CLI (promptYN)
import Data.Taskell.Task (Tasks, empty)

path :: FilePath
path = "taskell.json"

exists :: IO Bool
exists = doesFileExist path >>= promptCreate >>= return

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
readJSON = BS.readFile path >>= return . jsonToTasks

-- returns tasks or an empty list
jsonToTasks :: BS.ByteString -> Tasks
jsonToTasks s = case (decode s :: Maybe Tasks) of
    Just ts -> ts
    Nothing -> empty
