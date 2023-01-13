module Http.Response (Response(..)) where

import Network.HTTP.Types
import Prelude (Eq)
import Data.ByteString

data Response
    = Response
    { status :: Status
    , header :: ResponseHeaders
    , body :: ByteString
    }
    deriving (Eq)
