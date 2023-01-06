module Http.Action (Action, run, body, raw, param) where

import qualified Web.Scotty
import qualified Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue
import qualified Data.ByteString.Lazy
import qualified Data.ByteString
import qualified Control.Monad.IO.Class
import qualified Data.Text
import qualified Data.Text.Lazy

newtype Action a
    = Action (Web.Scotty.ActionM a)

instance Functor Action where
    fmap f (Action a) =
        Action (fmap f a)

instance Applicative Action where
    pure =
        Action . pure

    (<*>) (Action f) (Action a) =
        Action (f <*> a)

instance Monad Action where
    (>>=) (Action r) f =
        let
            bind =
                (>>=)

            f' a =
                let
                    (Action x) = f a
                in
                    x
        in
            Action (bind r f')




run :: Action a -> Web.Scotty.ActionM a
run (Action a) =
    a

body
    :: Control.Concurrent.STM.TQueue.TQueue Data.ByteString.ByteString
    -> Action ()
body q =
    Action $
    do
    lazy <- Web.Scotty.body
    let strict = Data.ByteString.Lazy.toStrict lazy
    Control.Monad.IO.Class.liftIO $
        Control.Concurrent.STM.atomically $
        Control.Concurrent.STM.TQueue.writeTQueue q strict


raw
    :: Control.Concurrent.STM.TQueue.TQueue Data.ByteString.ByteString
    -> Action ()
raw q =
    Action $
    do
    strict <-
        Control.Monad.IO.Class.liftIO $
        Control.Concurrent.STM.atomically $
        Control.Concurrent.STM.TQueue.readTQueue q
    let lazy = Data.ByteString.Lazy.fromStrict strict
    Web.Scotty.raw lazy


param
    :: Control.Concurrent.STM.TQueue.TQueue Data.Text.Text
    -> Data.Text.Text
    -> Action ()
param q key =
    Action $
    do
    let lazy = Data.Text.Lazy.fromStrict key
    value <- Web.Scotty.param lazy
    Control.Monad.IO.Class.liftIO $
        Control.Concurrent.STM.atomically $
        Control.Concurrent.STM.TQueue.writeTQueue q value
