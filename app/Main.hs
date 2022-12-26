module Main (main) where

import qualified Web.Scotty
import qualified Summaries

main :: IO ()
main =
  Web.Scotty.scotty 8080 $
    do
      Web.Scotty.get "/" $
        do
          Web.Scotty.setHeader "Content-Type" "text/html"
          Web.Scotty.file "index.html"

      Web.Scotty.get "/summaries" $
        do
          Web.Scotty.setHeader "Content-Type" "application/json"
          summaries <- Summaries.fromFile
          Web.Scotty.json summaries
