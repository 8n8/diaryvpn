module Request (Request (..)) where

import Data.ByteString
import Data.Text
import Network.HTTP.Types.Method

data Request = Request
  { path :: [Text],
    body :: [(ByteString, ByteString)],
    method :: Method,
    timestamp :: Int
  }
  deriving (Eq)
