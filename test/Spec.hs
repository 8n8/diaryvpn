module Main (main) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Diary (diary)
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Types.Status
import Request
import Response
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Hedgehog

indexHtml :: Text
indexHtml =
  "<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \  <head>\n\
  \    <meta charset=\"utf-8\" />\n\
  \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
  \    <title>Diary</title\n\
  \  </head>\n\
  \  <body>\n\
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
  defaultMain $
    testGroup "Tests" $
      [ testGroup "unit tests" $ Prelude.map oneTest cases,
        testGroup "property tests" properties
      ]

submittedHtml :: Text
submittedHtml =
  "<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \  <head>\n\
  \    <meta charset=\"utf8-8\" />\n\
  \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
  \    <title>Diary</title\n\
  \  </head>\n\
  \  <body>\n\
  \    <span>Your new diary entry has been saved.</span>\n\
  \  </body>\n\
  \</html>\n\
  \"

encodeUint32 :: Int -> Strict.ByteString
encodeUint32 i =
  Strict.pack $
    Prelude.map fromIntegral $
      Prelude.map (\x -> x .&. 0xff) $
        Prelude.map (\shift -> i `shiftR` shift) $
          Prelude.map (\x -> x * 8) $
            Prelude.take 4 [0 ..]

encodeEntry :: Int -> Text -> Strict.ByteString
encodeEntry timestamp entry =
  let encoded :: Strict.ByteString
      encoded = encodeUtf8 entry
   in encodeUint32 timestamp <> encodeUint32 (Strict.length encoded) <> encoded

properties :: [TestTree]
properties =
  [ Test.Tasty.Hedgehog.testProperty "new entry test" $
      Hedgehog.property $
        do
          entry <- Hedgehog.forAll $ Hedgehog.Gen.text (Range.constant 0 100) Hedgehog.Gen.unicode
          timestamp <- Hedgehog.forAll $ Hedgehog.Gen.integral (Range.constant 0 100000)
          let request =
                Request
                  { path = ["newentry"],
                    body = [("newentry", encodeUtf8 entry)],
                    method = "POST",
                    timestamp = timestamp
                  }
          let got = diary "" request
          let expected =
                ( Response
                    { headers = [("Content-Type", "text/html")],
                      status = ok200,
                      body = fromStrict $ encodeUtf8 submittedHtml
                    },
                  encodeEntry timestamp entry
                )
          got Hedgehog.=== expected
  ]

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
    ( "new entry route simple",
      "",
      Request
        { body = [("newentry", "I ate a ham sandwich")],
          path = ["newentry"],
          timestamp = 89,
          method = "POST"
        },
      ( Response
          { body = fromStrict $ encodeUtf8 submittedHtml,
            status = ok200,
            headers = [("Content-Type", "text/html")]
          },
        (Strict.pack [89, 0, 0, 0] <> Strict.pack [20, 0, 0, 0] <> "I ate a ham sandwich")
      )
    )
  ]
