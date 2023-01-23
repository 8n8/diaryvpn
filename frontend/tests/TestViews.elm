module TestViews exposing (suite)

import App exposing (Model)
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Test exposing (..)
import Test.Html.Selector as Selector
import Test.Html.Query as Query
import Expect exposing (Expectation)


suite : Test
suite =
    describe "tests"
        [ formAtStart
        , formHasDiaryTextBox
        ]


start : String -> ProgramTest Model () (SimulatedEffect ())
start initialUrl =
    ProgramTest.createApplication
        { onUrlChange = \_ -> ()
        , onUrlRequest = \_ -> ()
        , init = App.init
        , update = App.update
        , view = App.view
        }
        |> ProgramTest.withBaseUrl initialUrl
        |> ProgramTest.start ()


formAtStart : Test
formAtStart =
    test "form at start" <|
        \() ->
            start "https://example.com"
                |> ProgramTest.expectViewHas
                    [ Selector.tag "form"
                    ]


formHasDiaryTextBox : Test
formHasDiaryTextBox =
    test "form has diary text box" <|
        \() ->
            start "https://example.com"
                |> ProgramTest.expectView expectDiaryInputBox


expectDiaryInputBox : Query.Single () -> Expectation
expectDiaryInputBox =
    Query.find [Selector.tag "form"]
     >> Query.has [ Selector.tag "input" ]
