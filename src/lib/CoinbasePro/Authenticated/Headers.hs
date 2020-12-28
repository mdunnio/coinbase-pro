module CoinbasePro.Authenticated.Headers
    ( CBAccessKey(..)
    , CBAccessSign(..)
    , CBAccessTimeStamp(..)
    , CBAccessPassphrase(..)
    ) where

import           Data.Aeson         (ToJSON (..), Value (String))
import qualified Data.Aeson         as A
import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Web.HttpApiData    (ToHttpApiData (..))


newtype CBAccessKey = CBAccessKey Text
    deriving (Eq, Show)


instance ToHttpApiData CBAccessKey where
    toUrlPiece (CBAccessKey k)   = k
    toQueryParam (CBAccessKey k) = k


instance ToJSON CBAccessKey where
    toJSON (CBAccessKey k) = A.String k


newtype CBAccessSign = CBAccessSign ByteString
    deriving (Eq, Show)


instance ToHttpApiData CBAccessSign where
    toUrlPiece (CBAccessSign s)   = decodeUtf8 s
    toQueryParam (CBAccessSign s) = decodeUtf8 s


instance ToJSON CBAccessSign where
    toJSON (CBAccessSign s) = A.String $ decodeUtf8 s


newtype CBAccessTimeStamp = CBAccessTimeStamp Text
    deriving (Eq, Show)


instance ToHttpApiData CBAccessTimeStamp where
    toUrlPiece (CBAccessTimeStamp ts)   = ts
    toQueryParam (CBAccessTimeStamp ts) = ts


instance ToJSON CBAccessTimeStamp where
    toJSON (CBAccessTimeStamp ts) = String ts


newtype CBAccessPassphrase = CBAccessPassphrase Text
    deriving (Eq, Show)


instance ToJSON CBAccessPassphrase where
    toJSON (CBAccessPassphrase p) = A.String p


instance ToHttpApiData CBAccessPassphrase where
    toUrlPiece (CBAccessPassphrase p)   = p
    toQueryParam (CBAccessPassphrase p) = p
