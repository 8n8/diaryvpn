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
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        , view = view
        }


type Msg
    = DiaryBoxChange String
    | NoOp


init : () -> Url -> Browser.Navigation.Key -> ( String, Cmd Msg )
init () _ _ =
    ( "", Cmd.none )


update : Msg -> String -> ( String, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DiaryBoxChange new ->
            ( new, Cmd.none )


view : String -> Browser.Document Msg
view model =
    { title = "DiaryVPN"
    , body = [ Element.layout [] (body model) ]
    }


body : String -> Element Msg
body model =
    Element.column
        []
        [ Input.multiline
            []
            { onChange = DiaryBoxChange
            , text = model
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
