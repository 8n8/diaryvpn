module Http.Route (Route, run, get, post) where

import qualified Web.Scotty
import qualified Http.Action


newtype Route a
    = Route (Web.Scotty.ScottyM a)


run :: Route () -> Web.Scotty.ScottyM ()
run (Route r) =
    r

instance Functor Route where
    fmap f (Route a) =
        Route (fmap f a)


instance Applicative Route where
    pure =
        Route . pure

    (<*>) (Route f) (Route a) =
        Route (f <*> a)

instance Monad Route where
    (>>=) (Route r) f =
        let
            bind =
                (>>=)

            f' a =
                let
                    (Route x) = f a
                in
                    x
        in
            Route (bind r f')


get :: Web.Scotty.RoutePattern -> Http.Action.Action () -> Route ()
get route body =
    Route $ Web.Scotty.get route (Http.Action.run body)


post :: Web.Scotty.RoutePattern -> Http.Action.Action () -> Route ()
post route body =
    Route $ Web.Scotty.post route (Http.Action.run body)
