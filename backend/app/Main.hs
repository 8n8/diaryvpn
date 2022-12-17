module Main (main) where

import qualified Web.Scotty
import qualified Data.ByteString.Lazy
import qualified Control.Monad.IO.Class

main :: IO ()
main =
    Web.Scotty.scotty 8080 $
        do

        Web.Scotty.get "/" $
            do
            Web.Scotty.setHeader "Content-Type" "text/html"
            Web.Scotty.file "index.html"

        Web.Scotty.post "/api" $
            do
            body <- Web.Scotty.body
            Control.Monad.IO.Class.liftIO $
                Data.ByteString.Lazy.writeFile "db" body

        Web.Scotty.get "/api" $
            do
            Web.Scotty.setHeader "Content-Type" "application/octet-stream"
            Web.Scotty.file "db"
