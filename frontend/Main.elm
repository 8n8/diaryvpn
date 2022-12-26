module Main exposing (main)

import Html
import Browser
import Url exposing (Url)
import Browser.Navigation


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
    = Msg


type Model
    = Model


init : () -> Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ _ _ =
    (Model, Cmd.none)


view : Model -> Browser.Document Msg
view _ =
    { title = "DiaryElm"
    , body = [ Html.text "hi Elm!" ]
    }


update : Msg -> Model -> (Model, Cmd Msg)
update Msg Model =
    (Model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions Model =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    Msg


onUrlChange : Url -> Msg
onUrlChange _ =
    Msg
