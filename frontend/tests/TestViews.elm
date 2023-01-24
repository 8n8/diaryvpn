module TestViews exposing (suite)

import App exposing (Model)
import Expect exposing (Expectation)
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "tests"
        [ formAtStart
        , formHasDiaryTextBox
        , diaryTextBoxHasLabel
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
    Query.find [ Selector.tag "form" ]
        >> Query.has [ Selector.tag "textarea" ]


diaryTextBoxHasLabel : Test
diaryTextBoxHasLabel =
    test "diary text box has label" <|
        \() ->
            start "https://example.com"
                |> ProgramTest.expectView expectDiaryBoxLabel


expectDiaryBoxLabel : Query.Single () -> Expectation
expectDiaryBoxLabel =
    Query.find [ Selector.tag "form" ]
        >> Query.children []
        >> Query.first
        >> Query.find [ Selector.tag "label" ]
        >> Query.has [ Selector.text "Type your diary entry here:" ]
