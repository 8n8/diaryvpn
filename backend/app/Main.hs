module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (writeFile)
import Web.Scotty (body, file, get, post, scotty, setHeader)

rootPath :: FilePath
rootPath =
  "/home/t/.diaryvpn"

dbPath :: FilePath
dbPath =
  rootPath ++ "/db"

frontendPath :: FilePath
frontendPath =
  rootPath ++ "/index.html"

main :: IO ()
main =
  scotty 3000 $
    do
      get "/" $
        do
          setHeader "Content-Type" "text/html;charset=utf-8"
          file frontendPath

      get "/db" $
        do
          setHeader "Content-Type" "application/json"
          file dbPath

      post "/db" $
        do
          content <- body
          liftIO $ Data.ByteString.Lazy.writeFile dbPath content
