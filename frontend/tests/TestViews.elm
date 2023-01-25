module TestViews exposing (suite)

import App exposing (Model)
import Expect exposing (Expectation)
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Db


suite : Test
suite =
    describe "tests"
        [ init ]



init : Test
init =
    let
        backend = Db.empty
        frontend = App.init backend
        actualView = App.view frontend
        expectedView =
            { parents =
                
    in
        
