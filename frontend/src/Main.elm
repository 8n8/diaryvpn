module Main exposing (main)

import Browser
import Browser.Navigation
import Element exposing (Element)
import Element.Input as Input
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
    , body = [ Element.layout [] (body model) ]
    }


body : Model -> Element ()
body _ =
    Element.column
        []
        [ Input.multiline
            []
            { onChange = \_ -> ()
            , text = ""
            , placeholder = Nothing
            , label = Input.labelAbove [] (Element.text "Type your diary entry here:")
            , spellcheck = True
            }
        , Input.button
            []
            { onPress = Nothing
            , label = Element.text "Submit"
            }
        ]
