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

oneTest :: Case -> TestTree
oneTest c =
  testCase (description c) $
    diary (dbIn c) (request c) @?= (response c, dbOut c)

data Case = Case
  { description :: TestName,
    dbIn :: Strict.ByteString,
    request :: Request,
    response :: Response,
    dbOut :: Strict.ByteString
  }

cases :: [Case]
cases =
  [ Case
      { description = "root route",
        dbIn = "",
        request =
          Request
            { body = [],
              path = [],
              method = "GET",
              timestamp = 0
            },
        response =
          Response
            { body = fromStrict $ encodeUtf8 indexHtml,
              status = ok200,
              headers = [("Content-Type", "text/html")]
            },
        dbOut =
          ""
      },
    Case
      { description = "favicon route",
        dbIn = "",
        request =
          Request
            { body = [],
              path = ["favicon.ico"],
              method = "GET",
              timestamp = 0
            },
        response =
          Response
            { body = favicon,
              status = ok200,
              headers = [("Content-Type", "image/vnd.microsoft.icon")]
            },
        dbOut =
          ""
      },
    Case
      { description = "styles route",
        dbIn = "",
        request =
          Request
            { body = [],
              path = ["styles.css"],
              method = "GET",
              timestamp = 0
            },
        response =
          Response
            { body = fromStrict $ encodeUtf8 stylesCss,
              status = ok200,
              headers = [("Content-Type", "text/css")]
            },
        dbOut =
          ""
      }
  ]
