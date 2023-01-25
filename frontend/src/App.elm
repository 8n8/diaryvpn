module App exposing (Model(..), init, update, view)

import Browser
import Html exposing (Html)
import ProgramTest exposing (SimulatedEffect)
import SimulatedEffect.Cmd
import Url exposing (Url)


init : () -> Url -> () -> ( Model, SimulatedEffect () )
init () _ () =
    ( Model, SimulatedEffect.Cmd.none )


type Model
    = Model


update : () -> Model -> ( Model, SimulatedEffect () )
update _ _ =
    ( Model, SimulatedEffect.Cmd.none )


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
