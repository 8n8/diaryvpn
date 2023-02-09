module Main (main) where

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString
import qualified Data.Time.Clock.POSIX
import Diary
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Request
import Response
import System.IO.Error

dbPath :: FilePath
dbPath =
  "/home/t/.diaryvpn"

readDbFile :: IO (Either IOError Data.ByteString.ByteString)
readDbFile =
  try (Data.ByteString.readFile dbPath)

readDbIntoMemory :: IO (TVar ByteString)
readDbIntoMemory =
  do
    eitherRaw <- readDbFile
    let raw =
          ( case eitherRaw of
              Left err ->
                if isDoesNotExistError err
                  then ""
                  else Control.Exception.throw err
              Right ok ->
                ok
          ) ::
            ByteString

    atomically $ newTVar raw

now :: IO Int
now =
  do
    posix <- Data.Time.Clock.POSIX.getPOSIXTime
    return $ round posix

main :: IO ()
main =
  do
    dbTVar <- readDbIntoMemory
    Network.Wai.Handler.Warp.run 3456 $ \request responseWriter ->
      do
        let method = Network.Wai.requestMethod request
        let path = Network.Wai.pathInfo request
        (body, _) <-
          Network.Wai.Parse.parseRequestBody
            Network.Wai.Parse.lbsBackEnd
            request
        timestamp <- now
        (response, db) <- Control.Concurrent.STM.atomically $
          do
            oldDb <- readTVar dbTVar
            let (response, newDb) =
                  Diary.diary
                    oldDb
                    (Request {path, body, method, timestamp})
            writeTVar dbTVar newDb
            return (response, newDb)

        Data.ByteString.writeFile dbPath db

        responseWriter $
          Network.Wai.responseLBS
            (Response.status response)
            (Response.headers response)
            (Response.body response)
