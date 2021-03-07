{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module CoinbasePro.Authenticated.Oracle
    ( OracleResponse (..)
    ) where


import           Data.Aeson            (FromJSON, parseJSON, withObject, (.:))
import           Data.Aeson.Casing     (snakeCase)
import           Data.Aeson.TH         (defaultOptions, deriveJSON,
                                        fieldLabelModifier, unwrapUnaryRecords)
import           Data.Map.Strict       (Map)
import           Data.Text             (Text)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           CoinbasePro.Types     (CurrencyType, Price)


newtype Message = Message Text
  deriving Show


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''Message


newtype Signature = Signature Text
  deriving Show


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''Signature


data OracleResponse = OracleResponse
    { timestamp  :: UTCTime
    , messages   :: [Message]
    , signatures :: [Signature]
    , prices     :: Map CurrencyType Price
    } deriving Show


instance FromJSON OracleResponse where
  parseJSON = withObject "oracle response" $ \o -> OracleResponse
    <$> (posixSecondsToUTCTime . realToFrac . read <$> o .: "timestamp")
    <*> o .: "messages"
    <*> o .: "signatures"
    <*> o .: "prices"
