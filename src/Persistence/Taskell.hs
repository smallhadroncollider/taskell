module Persistence.Taskell where

import System.Directory
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BS

import UI.CLI (promptYN)
import Data.Taskell.Task (Tasks, empty)

path :: FilePath
path = "taskell.json"

exists :: IO Bool
exists = doesFileExist path >>= promptCreate

-- prompt whether to create taskell.json
promptCreate :: Bool -> IO Bool
promptCreate True = return True
promptCreate False = do
    cwd <- getCurrentDirectory
    r <- promptYN $ "Create " ++ cwd ++ "/" ++ path ++ "?"
    if r then createPath >> return True else return False

-- creates taskell file
createPath :: IO ()
createPath = writeFile path "[]"

-- writes Tasks to json file
writeJSON :: Tasks -> IO ()
writeJSON tasks = BS.writeFile "taskell.json" $ encodePretty tasks

-- reads json file
readJSON :: IO Tasks
readJSON = jsonToTasks <$> BS.readFile path

-- returns tasks or an empty list
jsonToTasks :: BS.ByteString -> Tasks
jsonToTasks s = fromMaybe empty (decode s :: Maybe Tasks)
