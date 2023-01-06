module Main (main) where

import App (
    Msg(Init, FileContents)
    , Cmd(None, ReadFile, Fork), Model, update, init,)
import Control.Concurrent (forkIO)
import Control.Exception (try)
import Prelude (IO, pure, ($), show)
import Control.Concurrent.STM.TVar (TVar, writeTVar, readTVar, newTVar)
import Control.Concurrent.STM (atomically)
import Data.ByteString (readFile)

main :: IO ()
main =
    do
    model <- atomically $ newTVar init
    updateIo Init model

updateIo :: Msg -> TVar Model -> IO ()
updateIo msg model =
    do
    cmd <-
        atomically $
        do
        oldModel <- readTVar model
        let (newModel, cmd) = update msg oldModel
        writeTVar model newModel
        pure cmd
    run cmd model

run :: Cmd -> TVar Model -> IO ()
run cmd model =
    case cmd of
    None ->
        pure ()

    ReadFile path ->
        do
        result <- try $ readFile $ show path
        updateIo (FileContents path result) model

    Fork forked ->
        do
        _ <- forkIO $ run forked model
        pure ()

    StartHttpServer ->
        
