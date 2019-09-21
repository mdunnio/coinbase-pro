{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module CoinbasePro.Headers
    ( RequiredHeader
    , UserAgent
    , UserAgentHeader
    , Before
    , After

    , userAgent
    ) where

import           Data.Text       (Text, pack, toLower)
import           Servant.API     (Header', Required)
import           Web.HttpApiData (ToHttpApiData (..))


type RequiredHeader = Header' '[Required]


newtype UserAgent = UserAgent Text
    deriving (Eq, Show, ToHttpApiData)


userAgent :: UserAgent
userAgent = UserAgent "coinbase-pro/0.4"


type UserAgentHeader = RequiredHeader "User-Agent" UserAgent


data Before = Before
    deriving (Eq, Show)


instance ToHttpApiData Before where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


data After = After
    deriving (Eq, Show)


instance ToHttpApiData After where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show
