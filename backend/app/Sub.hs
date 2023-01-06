module Sub (Sub(..), Sub.run) where

import Network.HTTP.Types
import Data.Binary.Builder
import Msg (Msg(..), Request(..))
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.ByteString
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status

data Sub
    = HttpServer Int


run :: Sub -> IO ()
run (HttpServer port) =
    do
    inQ <- liftIO $ atomically newTQueue
    Network.Wai.Handler.Warp.run port $ \request responseWriter ->
        case Network.Wai.requestBodyLength request of
        Network.Wai.ChunkedBody ->
            responseWriter $
                Network.Wai.responseBuilder
                    Network.HTTP.Types.Status.status411
                    []
                    Data.Binary.Builder.empty

        Network.Wai.KnownLength length' ->
            if length' > 2000 then
            responseWriter $
                Network.Wai.responseBuilder
                    Network.HTTP.Types.Status.status413
                    []
                    Data.Binary.Builder.empty

            else
            do
            outQ <- atomically newTQueue
            body' <- fmap toStrict $ Network.Wai.strictRequestBody request
            let headers' = Network.Wai.requestHeaders request
            let path' = Network.Wai.pathInfo request
            let query' = Network.Wai.queryString request
            let method' = Network.Wai.requestMethod request
            _ <- atomically $ writeTQueue inQ $
                Request {
                    body = body',
                    headers = headers',
                    path = path',
                    query = query',
                    method = method'
                }
            response <- atomically $ readTQueue outQ
            responseWriter $ 
              Network.Wai.responseBuilder
                (Sub.responseStatus response)
                (Sub.responseHeaders response)
                (Data.Binary.Builder.fromByteString (responseBody response))


data HttpResponse
    = HttpResponse
    { responseBody :: ByteString
    , responseStatus :: Network.HTTP.Types.Status
    , responseHeaders :: Network.HTTP.Types.ResponseHeaders
    }
