module Summaries (fromFile) where


import qualified Fragment
import qualified Timestamp
import qualified Data.ByteString
import qualified Data.Aeson


newtype Summaries
    = Summaries [Summary]


data Summary
    = Summary
    { timestamp :: Timestamp.Timestamp
    , fragment :: Fragment.Fragment
    }


instance FromJSON Summary where
    parseJSON = withObject "Summary" $ \v -> Summary
        <$> v.: "timestamp"
        <*> v .: "fragment"


fromFile :: IO Summaries
fromFile =
    do
    result <- Data.Aeson.eitherDecodeFileStrict' "db"
    case result of
        Left err ->
            error $ "failed to decode summaries file: " <> err

        Right summaries ->
            return summaries
