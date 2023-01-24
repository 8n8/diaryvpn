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


init : () -> Url -> Browser.Navigation.Key -> ( String, Cmd () )
init () _ _ =
    ( "", Cmd.none )


update : () -> String -> ( String, Cmd () )
update _ _ =
    ( "", Cmd.none )


view : String -> Browser.Document ()
view model =
    { title = "DiaryVPN"
    , body = [ Element.layout [] (body model) ]
    }


body : String -> Element ()
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
