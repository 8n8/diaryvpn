module Main exposing (main)

import Html
import Browser
import Url exposing (Url)
import Browser.Navigation
import Element exposing (Element)
import Summaries exposing (Summaries)


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
    = Loading
    | Loaded MainPage


type alias MainPage =
        { summaries : Summaries
        , textBox : String
        }


init : () -> Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ _ _ =
    (Loading, Cmd.none)


view : Model -> Browser.Document Msg
view model =
    { title = "DiaryElm"
    , body = [ Element.layout [] (viewBody model) ]
    }


viewBody : Model -> Element Msg
viewBody model =
    case model of
        Loading ->
            viewLoading

        Loaded loaded ->
            viewLoaded loaded





viewLoaded : Loaded -> Element Msg
viewLoaded { textBox, summaries } =
    [ viewTopBar
    , viewTextBox textBox
    , viewSummaries summaries
    ]
        |> Element.column []


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp
