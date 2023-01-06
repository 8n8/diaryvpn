module Main (main) where

import qualified Tea
import qualified Cmd
import qualified Http.Server
import qualified Msg
import qualified Http.Routes
import Data.ByteString
import qualified Http.Route

main :: IO ()
main =
    Tea.program init_ update


data Model
    = Fatal String
    | Ok OkModel


data OkModel
    = IndexHtml ByteString
    | Empty


init_ :: (Model, Cmd.Cmd)
init_ =
    ( Ok Empty
    , Cmd.ReadFile "index.html"
    )


update :: Msg.Msg -> Model -> (Model, Cmd.Cmd)
update msg model =
    case model of
        Fatal err ->
            (model, Cmd.None)

        Ok okModel ->
            updateOk msg okModel


updateOk :: Msg.Msg -> OkModel -> (Model, Cmd.Cmd)
updateOk msg model =
    case msg of
    Msg.None ->
        (Ok model, Cmd.StartHttpServer 3001)

    Msg.FileContents path (Left err) ->
        ( Fatal $
          mconcat
          [ "could not read file \""
          , path
          , "\": "
          , err
          ]
        , Cmd.None
        )

    Msg.FileContents "index.html" (Right contents) ->
        ( Ok $ IndexHtml contents , Cmd.None )

    Msg.Sequence [] ->
        (Ok model, Cmd.None)

    Msg.Sequence (first : remainder) ->
        let
            (model1, cmd1) = updateOk first model
            (model2, cmd2) = update (Msg.Sequence remainder) model1
        in
        (model2, Cmd.Sequence [cmd1, cmd2])

    Msg.HttpRequestQ httpQueue ->
        
