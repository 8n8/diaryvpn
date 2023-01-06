module Http.Routes (Routes, run, routes) where


import qualified Web.Scotty
import qualified Http.Route


newtype Routes
    = Routes (Web.Scotty.ScottyM ())


routes :: [Http.Route.Route ()] -> Routes
routes routes_ =
    Routes $ mapM_ Http.Route.run routes_ 


run :: Routes -> Web.Scotty.ScottyM ()
run (Routes r) =
    r
