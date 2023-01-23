module TestViews exposing (suite)

import App exposing (Model)
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Test exposing (..)
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "tests"
        [ testInit
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


testInit : Test
testInit =
    test "form at start" <|
        \() ->
            start "https://example.com"
                |> ProgramTest.expectViewHas
                    [ Selector.tag "form"
                    ]
