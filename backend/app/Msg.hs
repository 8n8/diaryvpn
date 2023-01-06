module Msg (Msg(..), Request(..)) where

import Data.ByteString (ByteString)
import Network.HTTP.Types
import Data.Text
import Control.Concurrent.STM.TQueue

data Msg
    = None
    | FileContents FilePath (Either String ByteString)
    | Sequence [Msg]
    | HttpRequestQ (TQueue Request)

data Request
    = Request
    { body :: ByteString
    , headers :: Network.HTTP.Types.RequestHeaders
    , path :: [Text]
    , query :: Network.HTTP.Types.Query
    , method :: Network.HTTP.Types.Method
    }
