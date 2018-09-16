module CoinbasePro.Headers
    ( CBAccessKey(..)
    , CBAccessSign(..)
    , CBAccessTimeStamp(..)
    , CBAccessPassphrase(..)
    ) where

import           Data.ByteString (ByteString)


newtype CBAccessKey = CBAccessKey { unCBAccessKey :: ByteString }
    deriving (Eq, Show, Ord)


newtype CBAccessSign = CBAccessSign { unCBAccessSign :: ByteString }
    deriving (Eq, Show, Ord)


newtype CBAccessTimeStamp = CBAccessTimeStamp { unCBAccessTimestamp :: String }
    deriving (Eq, Show, Ord)


newtype CBAccessPassphrase = CBAccessPassphrase { unCBAccessPassphrase :: ByteString }
    deriving (Eq, Show, Ord)
