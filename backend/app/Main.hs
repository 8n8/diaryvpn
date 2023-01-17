module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (writeFile)
import Web.Scotty (body, file, get, post, scotty, setHeader)

main :: IO ()
main =
  scotty 3000 $
    do
      get "/" $
        do
          setHeader "Content-Type" "text/html;charset=utf-8"
          file "dist/index.html"

      get "/elm.js" $
        do
          setHeader "Content-Type" "application/javascript"
          file "dist/elm.js"

      get "/db" $
        do
          setHeader "Content-Type" "application/json"
          file "db"

      post "/db" $
        do
          content <- body
          liftIO $ Data.ByteString.Lazy.writeFile "db" content
