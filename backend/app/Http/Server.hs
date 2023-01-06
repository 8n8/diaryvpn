module Http.Server (Server, server, run) where


import qualified Http.Routes
import qualified Web.Scotty


newtype Server
    = Server (IO ())


server :: Int -> Http.Routes.Routes -> Server
server port routes =
    Server (Web.Scotty.scotty port (Http.Routes.run routes))


run :: Server -> IO ()
run (Server s) =
    s
