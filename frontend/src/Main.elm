module Main exposing (main)

import Browser
import Browser.Navigation
import Element exposing (Element)
import Element.Input as Input
import Url exposing (Url)
import Element.Region as Region
import Http
import Db exposing (Db)


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
    | Submit
    | DbForSubmit String (Result Http.Error Db)


type Model
    = Write String
    | NoInternet
    | SubmitSuccess
    | InternalError String
    | Loading


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () _ _ =
    ( Write "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DiaryBoxChange new ->
            ( Write new, Cmd.none )

        Submit ->
            case model of
                Write newEntry ->
                    (Loading, submitEntry newEntry)

                NoInternet ->
                    (model, Cmd.none)

                SubmitSuccess ->
                    ( model, Cmd.none)

                InternalError _ ->
                    ( model, Cmd.none)

        DbForSubmit _ (Left err) ->
            case err of
                BadUrl


submitEntry : String -> Cmd Msg
submitEntry entry =
    Http.get
        { url = "/db"
        , expect = Http.expectJson (DbForSubmit entry) Db.decode
        }


view : Model -> Browser.Document Msg
view model =
    { title = "DiaryVPN"
    , body = [ Element.layout [] (body model) ]
    }


body : Model -> Element Msg
body model =
    case model of
        Write diary ->
            writePage diary

        NoInternet ->
            noInternet

        SubmitSuccess ->
            submitSuccess

        InternalError error ->
            internalError error


internalError : String -> Element Msg
internalError error =
    Element.column
        []
        [ Element.text "There was an internal error"
            |> Element.el [ Region.heading 1 ]
        , Element.paragraph
            []
            [ Element.text error
            ]
        ]


submitSuccess : Element Msg
submitSuccess =
    Element.column
        []
        [ Element.text "Success"
            |> Element.el [ Region.heading 1 ]
        , Element.paragraph
            []
            [ Element.text "You successfully submitted your diary entry."
            ]
        ]


noInternet : Element Msg
noInternet =
    Element.column
        []
        [ Element.text "No internet connection"
            |> Element.el [ Region.heading 1 ]
        , Element.paragraph
            []
            [ Element.text "Your internet connection is down. You could try refreshing the page"
            ]
        ]


writePage diary =
    Element.column
        []
        [ Input.multiline
            []
            { onChange = DiaryBoxChange
            , text = diary
            , placeholder = Nothing
            , label = Input.labelAbove [] (Element.text "Type your diary entry here:")
            , spellcheck = True
            }
        , Input.button
            []
            { onPress = Just Submit
            , label = Element.text "Submit"
            }
        ]
