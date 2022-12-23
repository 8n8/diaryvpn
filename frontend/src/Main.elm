module Main exposing (main)


import Browser
import Url exposing (Url)
import Browser.Navigation
import Html


main =
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
    }
        |> Browser.application


type Model
    = Model


init : () -> Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ _ _ =
    (Model, Cmd.none)


view : Model -> Browser.Document Msg
view model =
    { title = "hello Elm!"
    , body = [Html.text "hello Elm!"]
    }


type Msg
    = Msg


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
