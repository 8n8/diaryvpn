module Main (main) where

import qualified Web.Scotty

main :: IO ()
main =
    Web.Scotty.scotty 8080 $
        do

        Web.Scotty.get "/" $
            do
            Web.Scotty.setHeader "Content-Type" "text/html"
            Web.Scotty.file "index.html"

        Web.Scotty.get "/elm.js" $
            do
            Web.Scotty.setHeader "Content-Type" "text/html"
            Web.Scotty.file "elm.js"
