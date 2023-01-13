module App (init, update, Msg(..), Model(..), Cmd(..), Path(..)) where

import Prelude
    ( IOError, Either(Left, Right), mconcat, ($), String, show, Show
    , Eq
    , FilePath
    , Int
    , (++)
    , (<>)
    )
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Data.ByteString (ByteString, empty, singleton)
import Http.Request (Request(..), ApiRequest(..), parse)
import Data.Text (Text)
import Http.Response (Response(..))
import Http.RawRequest (RawRequest)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Data.Binary.Builder
import Network.HTTP.Types.Status
    (ok200
    , internalServerError500, notFound404)
import System.IO.Error (isDoesNotExistError)
import Network.HTTP.Types.Header (hContentEncoding)

data Cmd
    = None
    | ReadFile Path (Either IOError ByteString -> Msg)
    | StartHttpServer
    | Fork Cmd
    | WriteResponseQ (TQueue Response) Response
    | GetTime (UTCTime -> Msg)

data Path
    = IndexHtmlPath
    | ElmJsPath
    | EntryPath Int
    | SummariesPath
    deriving (Eq)

instance Show Path where
    show path =
        case path of
            IndexHtmlPath ->
                "index.html"

            ElmJsPath ->
                "elm.js"

data Model
    = Ok
    | Fatal String
    deriving (Eq, Show)


init :: Model
init =
    Ok


data Msg
    = Init
    | HttpRequest
        (TQueue Http.Response.Response)
        RawRequest
    | SummariesForResponse
        (TQueue Response)
        (Either IOError ByteString)
    | RawEntryFile Int (TQueue Response) (Either IOError ByteString)
    | EntryTime (TQueue Response) Text UTCTime
    | ElmJsResponse (TQueue Response) (Either IOError ByteString)
    | IndexHtmlResponse (TQueue Response) (Either IOError ByteString)
    | SummariesFile
        (TQueue Response)
        Text
        UTCTime
        (Either IOError ByteString)


update :: Msg -> Model -> (Model, Cmd)
update msg model =
    case model of
        Ok ->
            updateOk msg

        Fatal _ ->
            (model, None)


updateEmpty :: Msg -> (Model, Cmd)
updateEmpty msg =
    case msg of
        Init ->
            ( Ok, StartHttpServer )


updateOk :: Msg -> (Model, Cmd)
updateOk msg =
    case msg of
    ElmJsResponse q (Left err) ->
        (Fatal ("could read elm.js file: " <> show err)
        , WriteResponseQ
            q
            (Response
                { status = internalServerError500
                , header = []
                , body = Data.ByteString.empty
                })
        )

    SummariesFile q entry time (Left err) ->
        if isDoesNotExistError err then
            
        

    IndexHtmlResponse q (Left err) ->
        (Fatal ("could not read index.html file: " <> show err)
        , WriteResponseQ
            q
            (Response
                { status = internalServerError500
                , header = []
                , body = Data.ByteString.empty
                })
        )

    EntryTime q entry time ->
        (Ok, ReadFile SummariesPath (SummariesFile q entry time))
        
    RawEntryFile id q (Left err) ->
        if isDoesNotExistError err then
        (Ok
        , WriteResponseQ
            q
            (Response
                { status = notFound404
                , header = []
                , body = Data.ByteString.empty
                })
        )

        else
        ( Fatal $
            mconcat
            ["error reading file of entry with id "
            , show id
            , ": "
            , show err
            ]
        , WriteResponseQ
            q
            (Response
                { status = internalServerError500
                , header = []
                , body = Data.ByteString.empty
                })
        )
        
    Init ->
        (Ok, StartHttpServer)

    HttpRequest outQ request ->
        case Http.Request.parse request of
            Left errResponse ->
                (Ok, WriteResponseQ outQ errResponse)

            Right ok ->
                handleHttpRequest outQ ok

    SummariesForResponse q (Left err) ->
        if isDoesNotExistError err then
        (Ok
        , WriteResponseQ
            q
            (Response
                { status = ok200
                , header =
                    [(hContentEncoding, "application/octet-stream")]
                , body = Data.ByteString.singleton summaryResponse
                })
        )

        else
        ( Ok
        , WriteResponseQ
            q
            (Response
                { status = internalServerError500
                , header = []
                , body = Data.ByteString.empty
                })
        )

    SummariesForResponse q (Right rawSummaries) ->
        (Ok
        , WriteResponseQ
            q
            (Response
                { status = ok200
                , header = [(hContentEncoding, "application/octet-stream")]
                , body = rawSummaries
                })
        )


summaryResponse :: Word8
summaryResponse =
    0


handleHttpRequest
    :: TQueue Response
    -> Request
    -> (Model, Cmd)
handleHttpRequest q request =
    case request of
        Http.Request.Api body' ->
            handleApiRequest q body'

        Http.Request.IndexHtml ->
            (Ok, ReadFile IndexHtmlPath (IndexHtmlResponse q))

        Http.Request.ElmJs ->
            (Ok, ReadFile ElmJsPath (ElmJsResponse q))


handleApiRequest
    :: TQueue Response
    -> ApiRequest
    -> (Model, Cmd)
handleApiRequest q request =
    case request of
    UploadEntry entry ->
        (Ok, GetTime (EntryTime q entry))

    GetEntry id ->
        (Ok, ReadFile (EntryPath id) (RawEntryFile id q))

    GetSummaries ->
        (Ok, ReadFile SummariesPath (SummariesForResponse q))
