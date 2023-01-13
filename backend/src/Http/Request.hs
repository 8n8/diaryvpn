module Http.Request (Request(..), ApiRequest(..), parse) where

import Prelude (
    Either(..), ($), return, Int, fmap, fail, (*), (+), fromIntegral
    , show
    , (<>))
import qualified Http.RawRequest as RawRequest
import qualified Http.Response as Response
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.ByteString (ByteString, empty)
import Network.HTTP.Types.Status
import qualified Data.Attoparsec.ByteString as P

data Request
    = IndexHtml
    | ElmJs
    | Api ApiRequest


data ApiRequest
    = UploadEntry Text
    | GetEntry Int
    | GetSummaries


apiParser :: P.Parser ApiRequest
apiParser =
    P.choice
        [ uploadEntryParser
        , getEntryParser
        , getSummariesParser
        ]


uploadEntryParser :: P.Parser ApiRequest
uploadEntryParser =
    do
    _ <- P.word8 0
    raw <- P.takeByteString
    case decodeUtf8' raw of
        Left err ->
            fail $ "invalid UTF-8: " <> show err

        Right ok ->
            return $ UploadEntry ok


getEntryParser :: P.Parser ApiRequest
getEntryParser =
    do
    _ <- P.word8 1
    id <- parseId
    return $ GetEntry id


parseId :: P.Parser Int
parseId =
    do
    b0 <- fmap fromIntegral P.anyWord8
    b1 <- fmap fromIntegral P.anyWord8
    b2 <- fmap fromIntegral P.anyWord8
    b3 <- fmap fromIntegral P.anyWord8
    return $ b0 + (b1 * 256) + (b2 * 256 * 256) + (b3 * 256 * 256 * 256)


getSummariesParser :: P.Parser ApiRequest
getSummariesParser =
    do
    _ <- P.word8 2
    return GetSummaries


data Path
    = IndexHtmlPath
    | ElmJsPath
    | ApiPath

parsePath :: [Text] -> Path
parsePath path' =
    case path' of
    ["/api"] ->
        ApiPath

    ["/elmjs"] ->
        ElmJsPath

    _ ->
        IndexHtmlPath


parseApi :: ByteString -> Either Response.Response ApiRequest
parseApi raw =
    case P.parseOnly apiParser raw of
    Left _ ->
        Left $
        Response.Response
        { status = Network.HTTP.Types.Status.badRequest400
        , header = []
        , body = empty
        }

    Right ok ->
        Right ok


parse :: RawRequest.RawRequest -> Either Response.Response Request
parse request =
    do
    let path' = parsePath (RawRequest.path request)
    case path' of
        ApiPath ->
            fmap Api $ parseApi (RawRequest.body request)

        ElmJsPath ->
            return ElmJs

        IndexHtmlPath ->
            return IndexHtml
