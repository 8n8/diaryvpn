module Main (main) where

import qualified Web.Scotty
import qualified Diary
import qualified Db

main :: IO ()
main =
    do
    db <- Db.initDb

    Web.Scotty.scotty 8080 $
        do

        Web.Scotty.get "/" $
            do
            Web.Scotty.setHeader "Content-Type" "text/html"
            Web.Scotty.file "index.html"

        Web.Scotty.get "/summaries" $
            do
            Web.Scotty.setHeader "Content-Type" "application/octet-stream"
            summaries <- liftIO $ Db.query Diary.summaries db
            Web.Scotty.raw summaries
