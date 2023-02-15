module Diary (diary) where

import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Text
import Data.Text.Encoding
import qualified Data.Time
import qualified Data.Time.Clock
import qualified Data.Time.Clock.POSIX
import qualified Data.Time.Format
import qualified HTMLEntities.Text
import Network.HTTP.Types.Status
import qualified Request
import Response

data Request
  = Root
  | Favicon
  | Error404
  | Error400
  | Error500
  | NewEntry Int Text
  | Read

formatTime :: Int -> Text
formatTime i =
  let utc :: Data.Time.UTCTime
      utc = Data.Time.Clock.POSIX.posixSecondsToUTCTime $ Data.Time.Clock.secondsToNominalDiffTime (realToFrac i)
      str = Data.Time.Format.formatTime Data.Time.Format.defaultTimeLocale "%-I:%M%P %A %-d %B %Y" utc
   in Data.Text.pack str

parseRequest :: Request.Request -> Request
parseRequest (Request.Request path body _ timestamp) =
  case path of
    [] ->
      Root
    ["read"] ->
      Read
    ["favicon.ico"] ->
      Favicon
    ["newentry"] ->
      case body of
        [("newentry", binary)] ->
          if timestamp >= 0
            then case decodeUtf8' binary of
              Right text ->
                NewEntry timestamp text
              Left _ ->
                Error400
            else Error500
        _ ->
          Error400
    _ ->
      Error404

submittedHtml :: Text
submittedHtml =
  "<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \  <head>\n\
  \    <meta charset=\"utf-8\" />\n\
  \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
  \    <title>Diary</title>\n\
  \    <style>\n\
  \      span {\n\
  \        font-family: sans-serif;\n\
  \        font-size: 1.5rem;\n\
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
  \        font-family: sans-serif;\n\
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

readEntriesHtml :: [(Int, Text)] -> Text
readEntriesHtml entries =
  mconcat
    [ "<!DOCTYPE html>\n\
      \<html lang=\"en\">\n\
      \  <head>\n\
      \    <meta charset=\"utf-8\" />\n\
      \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
      \    <title>Diary</title>\n\
      \    <style>\n\
      \      html {\n\
      \        font-family: sans-serif;\n\
      \      }\n\
      \\n\
      \      h2 {\n\
      \        font-size: 1.5rem;\n\
      \        font-weight: 600;\n\
      \        margin: 0;\n\
      \      }\n\
      \      p {\n\
      \        font-size: 1.5rem;\n\
      \        font-weight: 400;\n\
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
      \    <div>\n",
      mconcat (Prelude.map viewEntry (Prelude.reverse entries)),
      "    </div>\n\
      \  </body>\n\
      \</html>\n"
    ]

viewEntry :: (Int, Text) -> Text
viewEntry (timestamp, entry) =
  let prettyTime = formatTime timestamp
      prettyText =
        Data.Text.intercalate "</p><p>" $
          Prelude.map HTMLEntities.Text.text $
            Data.Text.lines entry
   in mconcat
        [ "    <h2>" <> prettyTime <> "</h1>\n",
          "    <p>" <> prettyText <> "</p>\n"
        ]

parseDb :: Strict.ByteString -> Either String [(Int, Text)]
parseDb raw =
  Attoparsec.parseOnly dbParser raw

dbParser :: Attoparsec.Parser [(Int, Text)]
dbParser =
  Attoparsec.many' entryParser

entryParser :: Attoparsec.Parser (Int, Text)
entryParser =
  do
    t <- uint32Parser
    entry <- textParser
    return (t, entry)

textParser :: Attoparsec.Parser Text
textParser =
  do
    size <- uint32Parser
    bytes <- Attoparsec.take size
    case decodeUtf8' bytes of
      Left _ ->
        fail "invalid utf8"
      Right text ->
        return text

uint32Parser :: Attoparsec.Parser Int
uint32Parser =
  do
    b0 <- fmap fromIntegral Attoparsec.anyWord8
    b1 <- fmap fromIntegral Attoparsec.anyWord8
    b2 <- fmap fromIntegral Attoparsec.anyWord8
    b3 <- fmap fromIntegral Attoparsec.anyWord8
    return $ b0 + b1 `shiftL` 8 + b2 `shiftL` 16 + b3 `shiftL` 24

diary ::
  Strict.ByteString ->
  Request.Request ->
  (Response, Strict.ByteString)
diary db request =
  case parseRequest request of
    Read ->
      case parseDb db of
        Left _ ->
          ( Response
              { headers = [],
                body = Lazy.empty,
                status = internalServerError500
              },
            db
          )
        Right entries ->
          ( Response
              { headers = [("Content-Type", "text/html")],
                body = fromStrict $ encodeUtf8 $ readEntriesHtml entries,
                status = ok200
              },
            db
          )
    NewEntry text timestamp ->
      ( Response
          { headers = [("Content-Type", "text/html")],
            body = fromStrict $ encodeUtf8 submittedHtml,
            status = ok200
          },
        (db <> encodeEntry text timestamp)
      )
    Error404 ->
      ( Response
          { headers = [],
            body = Lazy.empty,
            status = notFound404
          },
        db
      )
    Error400 ->
      ( Response
          { headers = [],
            body = Lazy.empty,
            status = badRequest400
          },
        db
      )
    Error500 ->
      ( Response
          { headers = [],
            body = Lazy.empty,
            status = internalServerError500
          },
        db
      )
    Root ->
      ( Response
          { headers = [("Content-Type", "text/html")],
            body = fromStrict $ encodeUtf8 indexHtml,
            status = ok200
          },
        db
      )
    Favicon ->
      ( Response
          { headers = [("Content-Type", "image/vnd.microsoft.icon")],
            status = ok200,
            body = favicon
          },
        db
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
