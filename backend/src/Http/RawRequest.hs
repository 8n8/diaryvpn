module Http.RawRequest (RawRequest(..), fromWai) where

import Network.HTTP.Types
import qualified Network.Wai
import Data.ByteString.Lazy (toStrict)
import Data.Word (Word64)
import Data.ByteString (ByteString, empty)
import Prelude (IO, Either(..), ($), return, fmap, (>), (*))
import Data.Text (Text)
import Http.Response (Response(..))

data RawRequest
    = RawRequest
        { path :: [Text]
        , body :: ByteString
        }

maxBodyLength :: Word64
maxBodyLength =
    100*1000

fromWai :: Network.Wai.Request -> IO (Either Response RawRequest)
fromWai request =
    case Network.Wai.requestBodyLength request of
        Network.Wai.ChunkedBody ->
            return $
            Left $
            Response
            { status = Network.HTTP.Types.lengthRequired411
            , header = []
            , body = Data.ByteString.empty
            }

        Network.Wai.KnownLength length ->
            if length > maxBodyLength then
            return $
            Left $
            Response
            { status = Network.HTTP.Types.requestEntityTooLarge413
            , header = []
            , body = Data.ByteString.empty
            }

            else
            do
            body' <- fmap toStrict (Network.Wai.strictRequestBody request)
            return $
                Right $
                RawRequest
                { body = body'
                , path = Network.Wai.pathInfo request
                }
