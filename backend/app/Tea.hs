module Tea (program) where

import qualified Cmd
import Msg
import Sub

program
    :: (model, Cmd.Cmd)
    -> Sub.Sub
    -> (Msg -> model -> (model, Cmd.Cmd))
    -> IO ()
program (oldModel, oldCmd) subscriptions update =
    do
    forkIO $ Sub.run subscriptions
    msg <- Cmd.run oldCmd
    let (newModel, newCmd) = update msg oldModel
    program (newModel, newCmd) update
