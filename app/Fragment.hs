module Fragment (Fragment) where


import qualified Data.Text


newtype Fragment
    = Fragment Data.Text.Text


instance FromJSON Fragment where
    parseJSON = withObject "Fragment" $ \v -> Fragment
        <$> v.:
