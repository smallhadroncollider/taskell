{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Taskell where

import ClassyPrelude

import System.Directory (getCurrentDirectory, doesFileExist)

import Data.Taskell.Lists (Lists, initial)
import IO.Config (Config, general, filename)
import IO.Markdown (stringify, parse)
import UI.CLI (promptYN)

type ReaderConfig a = ReaderT Config IO a

getPath :: ReaderConfig FilePath
getPath = do
    config <- ask
    let defaultPath = filename $ general config
    mArgs <- fromNullable <$> getArgs
    return $ case mArgs of
        Just args -> unpack $ head args
        Nothing -> defaultPath

exists :: ReaderConfig (Bool, FilePath)
exists = do
    path <- getPath
    exists' <- lift $ doesFileExist path
    success <- promptCreate exists' path
    return (success, path)

-- prompt whether to create taskell.json
promptCreate :: Bool -> FilePath -> ReaderConfig Bool
promptCreate True _ = return True
promptCreate False path = do
    cwd <- lift $ pack <$> getCurrentDirectory
    create <- lift $ promptYN $ "Create " ++ cwd ++ "/" ++ pack path ++ "?"
    if create then createPath path >> return True else return False

-- creates taskell file
createPath :: FilePath -> ReaderConfig ()
createPath path = do
    config <- ask
    lift (writeData config initial path)

-- writes Tasks to json file
writeData :: Config -> Lists -> FilePath -> IO ()
writeData config tasks path = void (writeFile path $ stringify config tasks)

-- reads json file
readData :: Config -> FilePath -> IO (Either Text Lists)
readData config path = do
    content <- readFile path
    return $ parse config content
