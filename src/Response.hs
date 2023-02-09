module Response (Response (..)) where

import Data.ByteString.Lazy
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

data Response = Response
  { headers :: [Header],
    status :: Status,
    body :: ByteString
  }
  deriving (Eq, Show)
