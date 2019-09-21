module CoinbasePro.Authenticated.Headers
    ( CBAccessKey(..)
    , CBAccessSign(..)
    , CBAccessTimeStamp(..)
    , CBAccessPassphrase(..)
    ) where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text, pack)
import           Data.Text.Encoding (decodeUtf8)
import           Web.HttpApiData    (ToHttpApiData (..))


newtype CBAccessKey = CBAccessKey String
    deriving (Eq, Show)


instance ToHttpApiData CBAccessKey where
    toUrlPiece (CBAccessKey k)   = pack k
    toQueryParam (CBAccessKey k) = pack k


newtype CBAccessSign = CBAccessSign ByteString
    deriving (Eq, Show)


instance ToHttpApiData CBAccessSign where
    toUrlPiece (CBAccessSign s)   = decodeUtf8 s
    toQueryParam (CBAccessSign s) = decodeUtf8 s


newtype CBAccessTimeStamp = CBAccessTimeStamp String
    deriving (Eq, Show)


instance ToHttpApiData CBAccessTimeStamp where
    toUrlPiece (CBAccessTimeStamp ts)   = pack ts
    toQueryParam (CBAccessTimeStamp ts) = pack ts


newtype CBAccessPassphrase = CBAccessPassphrase String
    deriving (Eq, Show)


instance ToHttpApiData CBAccessPassphrase where
    toUrlPiece (CBAccessPassphrase p)   = pack p
    toQueryParam (CBAccessPassphrase p) = pack p
