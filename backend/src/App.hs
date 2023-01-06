module App (init, update, Msg(..), Model(..), Cmd(..)) where

import Prelude
    ( IOError, Either(Left, Right), mconcat, ($), String, show, Show
    , Maybe(Just, Nothing)
    )
import Data.ByteString (ByteString)

data Cmd
    = None
    | ReadFile Path
    | StartHttpServer
    | Fork Cmd

data Path
    = IndexHtml
    | ElmJs

instance Show Path where
    show path =
        case path of
            IndexHtml ->
                "index.html"

            ElmJs ->
                "elm.js"

data Model
    = Ok OkModel
    | Fatal String


init :: Model
init =
    Ok $ OkModel { indexHtml = Nothing, elmJs = Nothing }


data OkModel
    = OkModel
    { indexHtml :: Maybe ByteString
    , elmJs :: Maybe ByteString
    }

data Msg
    = Init
    | FileContents Path (Either IOError ByteString)

update :: Msg -> Model -> (Model, Cmd)
update msg model =
    case model of
        Ok ok ->
            updateOk msg ok

        Fatal _ ->
            (model, None)


updateOk :: Msg -> OkModel -> (Model, Cmd)
updateOk msg model =
    case msg of
    Init ->
        ( Ok model , ReadFile IndexHtml)

    FileContents path (Left err) ->
        (Fatal $
         mconcat
         [ "could not read file \""
         , show path
         , "\": "
         , show err
         ]
        , None
        )

    FileContents IndexHtml (Right contents) ->
        ( Ok $ model { indexHtml = Just contents } , ReadFile ElmJs)
            
    FileContents ElmJs (Right contents) ->
        ( Ok $ model { elmJs = Just contents }, StartHttpServer )
