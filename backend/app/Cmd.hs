module Cmd (Cmd(..), Cmd.run) where

import Data.ByteString
import Msg (Msg(..))
import Control.Exception (try)

data Cmd
    = None
    | Sequence [Cmd]
    | ReadFile FilePath


mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f either' =
    case either' of
        Left e1 ->
            Left $ f e1

        Right a ->
            Right a


run :: Cmd -> IO Msg
run cmd =
    case cmd of
    Cmd.None ->
        return Msg.None

    Cmd.Sequence cmds ->
        do
        results <- mapM Cmd.run cmds
        return $ Msg.Sequence results

    ReadFile path' ->
        do
        result <- (try $ Data.ByteString.readFile path') :: IO (Either IOError ByteString)
        return $ FileContents path' (mapLeft show result)
