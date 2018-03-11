{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Taskell where

import ClassyPrelude as P

import System.Directory (getCurrentDirectory, doesFileExist)

import Data.Taskell.Lists (Lists, initial)
import IO.Config (Config, general, filename)
import IO.Markdown (stringify, parse)
import UI.CLI (promptYN)

getPath :: Config -> IO FilePath
getPath c = do
    let defaultPath = filename $ general c
    mArgs <- fromNullable <$> getArgs
    return $ case mArgs of
        Just args -> unpack $ head args
        Nothing -> defaultPath

exists :: Config -> IO (Bool, FilePath)
exists c = do
    path <- getPath c
    e <- doesFileExist path
    success <- promptCreate c e path
    return (success, path)

-- prompt whether to create taskell.json
promptCreate :: Config -> Bool -> FilePath -> IO Bool
promptCreate _ True _ = return True
promptCreate config False path = do
    cwd <- pack <$> getCurrentDirectory
    r <- promptYN $ "Create " ++ cwd ++ "/" ++ pack path ++ "?"
    if r then createPath config path >> return True else return False

-- creates taskell file
createPath :: Config -> FilePath -> IO ()
createPath config = writeData config initial

-- writes Tasks to json file
writeData :: Config -> Lists -> FilePath -> IO ()
writeData config tasks path = void (P.writeFile path $ stringify config tasks)

-- reads json file
readData :: Config -> FilePath -> IO (Either Text Lists)
readData config path = do
    content <- P.readFile path
    return $ parse config content
