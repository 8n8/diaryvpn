module TestViews exposing (suite)

import App exposing (Model, Msg)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Test exposing (..)
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "tests"
        [ testInit
        ]


start : String -> ProgramTest Model Msg (SimulatedEffect Msg)
start initialUrl =
    ProgramTest.createApplication
        { onUrlChange = App.OnUrlChange
        , onUrlRequest = App.OnUrlRequest
        , init =
            -- NOTE: the type of MyProgram.init is:
            -- MyProgram.Flags -> Navigation.Location -> (MyProgram.Model, Cmd MyProgram.Msg)
            App.init
        , update = App.update
        , view = App.view
        }
        |> ProgramTest.withBaseUrl initialUrl
        |> ProgramTest.start ()


testInit : Test
testInit =
    test "form at start" <|
        \() ->
            start "https://example.com"
                |> ProgramTest.expectViewHas
                    [ Selector.tag "form"
                    ]
