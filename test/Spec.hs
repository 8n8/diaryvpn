module Main (main) where

import Data.ByteString as Strict
import Data.Text.Encoding (encodeUtf8)
import Diary (diary)
import Favicon (favicon)
import IndexHtml (indexHtml)
import Network.HTTP.Types.Status
import Request
import Response
import StylesCss (stylesCss)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude
  ( IO,
    map,
    ($),
  )

main :: IO ()
main =
  defaultMain $ testGroup "unit tests" $ Prelude.map oneTest cases

oneTest ::
  (TestName, Strict.ByteString, Request, (Response, Strict.ByteString)) ->
  TestTree
oneTest (name, db, request, expected) =
  testCase name $ diary db request @?= expected

cases :: [(TestName, Strict.ByteString, Request, (Response, Strict.ByteString))]
cases =
  [ ( "root route",
      "",
      Request
        { body = [],
          path = [],
          method = "GET",
          timestamp = 0
        },
      ( Response
          { body = fromStrict $ encodeUtf8 indexHtml,
            status = ok200,
            headers = [("Content-Type", "text/html")]
          },
        ""
      )
    ),
    ( "favicon route",
      "",
      Request
        { body = [],
          path = ["favicon.ico"],
          method = "GET",
          timestamp = 0
        },
      ( Response
          { body = favicon,
            status = ok200,
            headers = [("Content-Type", "image/vnd.microsoft.icon")]
          },
        ""
      )
    ),
    ( "styles route",
      "",
      Request
        { body = [],
          path = ["styles.css"],
          method = "GET",
          timestamp = 0
        },
      ( Response
          { body = fromStrict $ encodeUtf8 stylesCss,
            status = ok200,
            headers = [("Content-Type", "text/css")]
          },
        ""
      )
    )
  ]
