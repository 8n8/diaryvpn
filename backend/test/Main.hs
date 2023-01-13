module Main (main) where

import App (update, Msg(..), Model(..), Cmd(ReadFile), Path(..))
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import Prelude (IO, ($), map, String)

main :: IO ()
main =
  Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup "Unit tests" $
    map oneTest cases

oneTest :: (String, Msg, Model, (Model, Cmd)) -> Test.Tasty.TestTree
oneTest (name, msg, model, expected) =
  Test.Tasty.HUnit.testCase name $
    (update msg model) Test.Tasty.HUnit.@?= expected

cases :: [(String, Msg, Model, (Model, Cmd))]
cases =
  [ ( "Initialisation",
      Init,
      Empty,
      (Empty, ReadFile IndexHtml)
    )
  ]
