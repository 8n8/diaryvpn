module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html)
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> ()
        , onUrlRequest = \_ -> ()
        , view = view
        }


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd () )
init () _ _ =
    ( Model, Cmd.none )


type Model
    = Model


update : () -> Model -> ( Model, Cmd () )
update _ _ =
    ( Model, Cmd.none )


view : Model -> Browser.Document ()
view model =
    { title = "DiaryVPN"
    , body = body model
    }


body : Model -> List (Html ())
body _ =
    [ Html.form
        []
        [ Html.div
            []
            [ Html.textarea [] []
            , Html.label [] [ Html.text "Type your diary entry here:" ]
            ]
        ]
    ]
