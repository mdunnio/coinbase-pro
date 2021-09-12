{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Deposit
    ( DepositDetails (..)
    , DepositRequest (..)
    , DepositResponse (..)
    , CoinbaseDepositRequest (..)
    , CryptoDepositAddress (..)
    ) where

import           Data.Aeson                         (FromJSON,
                                                     parseJSON, withObject, (.:))
import           Data.Aeson.Casing                  (snakeCase)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import           Data.UUID                          (UUID)

import           CoinbasePro.Authenticated.Accounts (AccountId)
import           CoinbasePro.Authenticated.Payment  (PaymentMethodId (..))
import           CoinbasePro.Types                  (CreatedAt)


data DepositDetails = DepositDetails
    { cryptoAddress         :: Maybe Text
    , destinationTag        :: Maybe Text
    , coinbaseAccountId     :: AccountId
    , destinationTagName    :: Maybe Text
    , coinbaseTransactionId :: Text
    , cryptoTransactionHash :: Maybe Text
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''DepositDetails


data DepositRequest = DepositRequest
    { amount          :: Double
    , currency        :: Text
    , paymentMethodId :: PaymentMethodId
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''DepositRequest


data CoinbaseDepositRequest = CoinbaseDepositRequest
    { cAmount            :: Double
    , cCurrency          :: Text
    , cCoinbaseAccountId :: AccountId
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 1 } ''CoinbaseDepositRequest


data DepositResponse = DepositResponse
    { rId       :: UUID
    , rAmount   :: Double
    , rCurrency :: Text
    , rPayoutAt :: UTCTime
    } deriving (Eq, Show)

instance FromJSON DepositResponse where
    parseJSON = withObject "deposit response" $ \o ->
        DepositResponse
        <$> o .: "id"
        <*> (read <$> o .: "amount")
        <*> o .: "currency"
        <*> o .: "payout_at"

data AddressInfo = AddressInfo
    { aiAddress        :: Text
    , aiDestinationTag :: Maybe Int
    } deriving Show


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 2 } ''AddressInfo


data CryptoDepositAddress = CryptoDepositAddress
    { cdaId                     :: UUID
    , cdaAddress                :: Text
    , cdaDestinationTag         :: Maybe Int
    , cdaAddressInfo            :: AddressInfo
    , cdaCreatedAt              :: CreatedAt
    , cdaUpdatedAt              :: UTCTime
    , cdaNetwork                :: Text
    , cdaResource               :: Text
    , cdaDepositUri             :: Text
    , cdaExchangeDepositAddress :: Bool
    } deriving Show


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 3 } ''CryptoDepositAddress
