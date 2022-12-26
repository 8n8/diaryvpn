module Main exposing (main)

import Html
import Browser
import Url exposing (Url)
import Browser.Navigation
import Element exposing (Element)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type Msg
    = NoOp


type Model
    = Model


init : () -> Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ _ _ =
    (Model, Cmd.none)


view : Model -> Browser.Document Msg
view model =
    { title = "DiaryElm"
    , body = [ Element.layout [] (viewBody model) ]
    }


viewBody : Model -> Element Msg
viewBody Model =
    Element.text "Hello Elm-UI"


update : Msg -> Model -> (Model, Cmd Msg)
update NoOp Model =
    (Model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions Model =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp
