module Main (main) where

import App (update, Msg(..), Model(..), Cmd(ReadFile))
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import Prelude (IO)

main :: IO ()
main =
  Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup "Unit tests" $
    map oneTest cases

oneTest (name, msg, model, expected) =
  Test.Tasty.HUnit.testCase name $
    (update msg model) Test.Tasty.HUnit.@?= expected

cases =
  [ ( "Initialisation",
      Init,
      Empty,
      (Empty, ReadFile "index.html")
    )
  ]
