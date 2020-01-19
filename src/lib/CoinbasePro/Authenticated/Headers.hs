module CoinbasePro.Authenticated.Headers
    ( CBAccessKey(..)
    , CBAccessSign(..)
    , CBAccessTimeStamp(..)
    , CBAccessPassphrase(..)
    ) where

import           Data.Aeson         (ToJSON (..), Value (String))
import           Data.ByteString    (ByteString)
import           Data.Text          (pack)
import           Data.Text.Encoding (decodeUtf8)
import           Web.HttpApiData    (ToHttpApiData (..))


newtype CBAccessKey = CBAccessKey String
    deriving (Eq, Show)


instance ToHttpApiData CBAccessKey where
    toUrlPiece (CBAccessKey k)   = pack k
    toQueryParam (CBAccessKey k) = pack k


instance ToJSON CBAccessKey where
    toJSON (CBAccessKey k) = String $ pack k


newtype CBAccessSign = CBAccessSign ByteString
    deriving (Eq, Show)


instance ToHttpApiData CBAccessSign where
    toUrlPiece (CBAccessSign s)   = decodeUtf8 s
    toQueryParam (CBAccessSign s) = decodeUtf8 s


instance ToJSON CBAccessSign where
    toJSON (CBAccessSign s) = String $ decodeUtf8 s


newtype CBAccessTimeStamp = CBAccessTimeStamp String
    deriving (Eq, Show)


instance ToHttpApiData CBAccessTimeStamp where
    toUrlPiece (CBAccessTimeStamp ts)   = pack ts
    toQueryParam (CBAccessTimeStamp ts) = pack ts


instance ToJSON CBAccessTimeStamp where
    toJSON (CBAccessTimeStamp ts) = String $ pack ts


newtype CBAccessPassphrase = CBAccessPassphrase String
    deriving (Eq, Show)


instance ToJSON CBAccessPassphrase where
    toJSON (CBAccessPassphrase p) = String $ pack p


instance ToHttpApiData CBAccessPassphrase where
    toUrlPiece (CBAccessPassphrase p)   = pack p
    toQueryParam (CBAccessPassphrase p) = pack p
