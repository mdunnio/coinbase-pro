{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module CoinbasePro.Headers
    ( RequiredHeader
    , UserAgent
    , UserAgentHeader

    , userAgent
    ) where

import           Data.Text       (Text)
import           Servant.API     (Header', Required)
import           Web.HttpApiData (ToHttpApiData (..))


type RequiredHeader = Header' '[Required]


newtype UserAgent = UserAgent Text
    deriving (Eq, Show, ToHttpApiData)


userAgent :: UserAgent
userAgent = UserAgent "coinbase-pro/0.4"


type UserAgentHeader = RequiredHeader "User-Agent" UserAgent
