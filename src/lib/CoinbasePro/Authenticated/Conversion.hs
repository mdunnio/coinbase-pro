{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.Authenticated.Conversion
  ( ConversionId
  , StablecoinConversionRequest (..)
  , StablecoinConversionResponse (..)
  ) where

import           Data.Aeson                         (FromJSON, parseJSON,
                                                     withObject, (.:))
import           Data.Aeson.Casing                  (snakeCase)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier,
                                                     unwrapUnaryRecords)
import           Data.UUID                          (UUID, toString)

import           CoinbasePro.Authenticated.Accounts (AccountId)
import           CoinbasePro.Types                  (CurrencyType)


data StablecoinConversionRequest = StablecoinConversionRequest
    { reqFrom   :: CurrencyType
    , reqTo     :: CurrencyType
    , reqAmount :: Double
    } deriving Show


deriveJSON defaultOptions
  { fieldLabelModifier = snakeCase . drop 3
  } ''StablecoinConversionRequest


newtype ConversionId = ConversionId UUID


instance Show ConversionId where
  show (ConversionId u) = toString u


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    , unwrapUnaryRecords = True
    } ''ConversionId


data StablecoinConversionResponse = StablecoinConversionResponse
    { resId            :: ConversionId
    , resAmount        :: Double
    , resFromAccountId :: AccountId
    , resToAccountId   :: AccountId
    , resFrom          :: CurrencyType
    , restTo           :: CurrencyType
    } deriving Show


instance FromJSON StablecoinConversionResponse where
  parseJSON = withObject "stablecoin conversion response" $ \o -> StablecoinConversionResponse
    <$> o .: "id"
    <*> (read <$> o .: "amount")
    <*> o .: "from_account_id"
    <*> o .: "to_account_id"
    <*> o .: "from"
    <*> o .: "to"
