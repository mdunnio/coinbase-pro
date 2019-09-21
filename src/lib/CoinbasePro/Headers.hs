{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module CoinbasePro.Headers
    ( RequiredHeader
    , UserAgent
    , UserAgentHeader
    , CBAccessKey(..)
    , CBAccessSign(..)
    , CBAccessTimeStamp(..)
    , CBAccessPassphrase(..)

    , userAgent
    ) where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text, pack)
import           Data.Text.Encoding (decodeUtf8)
import           Servant.API        (Header', Required)
import           Web.HttpApiData    (ToHttpApiData (..))


type RequiredHeader = Header' '[Required]


newtype UserAgent = UserAgent Text
    deriving (Eq, Show, ToHttpApiData)


userAgent :: UserAgent
userAgent = UserAgent "coinbase-pro/0.4"


type UserAgentHeader = RequiredHeader "User-Agent" UserAgent


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
