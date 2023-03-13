module Main (main) where

import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Diary (diary)
import Network.HTTP.Types.Status
import Request
import Response
import Test.Tasty
import Test.Tasty.HUnit
import Prelude
  ( IO,
    map,
    ($),
  )

indexHtml :: Text
indexHtml =
  "<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \  <head>\n\
  \    <meta charset=\"utf-8\" />\n\
  \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
  \    <title>Diary</title>\n\
  \    <style>\n\
  \      html {\n\
  \        font-family: sans-serif;\n\
  \      }\n\
  \      label {\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \      form {\n\
  \        display: flex;\n\
  \        flex-direction: column;\n\
  \        row-gap: 0.5rem;\n\
  \      }\n\
  \      textarea {\n\
  \        font-family: sans-serif;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \      textarea:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      input {\n\
  \        width: fit-content;\n\
  \        font-size: 1.5rem;\n\
  \        font-family: sans-serif;\n\
  \      }\n\
  \      input:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      a:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      body {\n\
  \        display: flex;\n\
  \        flex-direction: column;\n\
  \        row-gap: 1rem;\n\
  \      }\n\
  \      ul {\n\
  \        display: flex;\n\
  \        flex-direction: row;\n\
  \        list-style-type: none;\n\
  \        padding: 0;\n\
  \        margin: 0;\n\
  \        column-gap: 1rem;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \    </style>\n\
  \  </head>\n\
  \  <body>\n\
  \    <nav>\n\
  \      <ul>\n\
  \        <li><a href=\"/read\">Read</a></li>\n\
  \        <li><a href=\"/\">Write</a></li>\n\
  \      </ul>\n\
  \    </nav>\n\
  \    <form action=\"/newentry\" method=\"POST\">\n\
  \      <label for=\"newentry\">Type a new diary entry here:</label>\n\
  \      <textarea\n\
  \        id=\"newentry\"\n\
  \        name=\"newentry\"\n\
  \        rows=\"5\"\n\
  \        cols=\"20\"></textarea>\n\
  \      <input type=\"submit\" value=\"Submit\">\n\
  \    </form>\n\
  \  </body>\n\
  \</html>\n\
  \"

favicon :: Lazy.ByteString
favicon =
  Lazy.pack
    [ 0x00,
      0x00,
      0x01,
      0x00,
      0x01,
      0x00,
      0x10,
      0x10,
      0x10,
      0x00,
      0x01,
      0x00,
      0x04,
      0x00,
      0x28,
      0x01,
      0x00,
      0x00,
      0x16,
      0x00,
      0x00,
      0x00,
      0x28,
      0x00,
      0x00,
      0x00,
      0x10,
      0x00,
      0x00,
      0x00,
      0x20,
      0x00,
      0x00,
      0x00,
      0x01,
      0x00,
      0x04,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0xed,
      0x29,
      0xdd,
      0x00,
      0xb6,
      0x6f,
      0x3e,
      0x00,
      0x0d,
      0xf4,
      0x0b,
      0x00,
      0x4a,
      0xf2,
      0xf5,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00
    ]

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
    )
  ]
