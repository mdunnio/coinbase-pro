{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.Authenticated.Withdrawal
    ( WithdrawalDetails (..)
    , WithdrawalRequest (..)
    , WithdrawalResponse (..)
    , CoinbaseWithdrawalRequest (..)
    , CryptoWithdrawalRequest (..)
    , CryptoWithdrawalResponse (..)
    , WithdrawalFeeEstimateResponse (..)
    ) where

import           Data.Aeson                         (FromJSON, parseJSON,
                                                     withObject, (.:), (.:?))
import           Data.Aeson.Casing                  (snakeCase)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import           Data.UUID                          (UUID)

import           CoinbasePro.Authenticated.Accounts (AccountId)
import           CoinbasePro.Authenticated.Payment  (PaymentMethodId)
import           Control.Applicative


data WithdrawalDetails = WithdrawalDetails
    { destinationTag        :: Maybe Text
    , sentToAddress         :: Maybe Text
    , coinbaseAccountId     :: Text
    , destinationTagName    :: Maybe Text
    , coinbaseWithdrawalId  :: Maybe Text
    , coinbaseTransactionId :: Maybe Text
    , cryptoPaymentMethodId :: Text
    , fee                   :: Maybe Double
    , subtotal              :: Maybe Double
    } deriving (Eq, Show)


instance FromJSON WithdrawalDetails where
  parseJSON = withObject "withdrawal details" $ \o -> WithdrawalDetails
    <$> o .:? "destination_tag"
    <*> o .:? "sent_to_address"
    <*> o .: "coinbase_account_id"
    <*> o .:? "destination_tag_name"
    <*> o .:? "coinbase_withdrawal_id"
    <*> o .:? "coinbase_transaction_id"
    <*> o .: "coinbase_payment_method_id"
    <*> (maybe Nothing read <$> o .:? "fee")
    <*> (maybe Nothing read <$> o .:? "subtotal")


data WithdrawalRequest = WithdrawalRequest
    { amount          :: Double
    , currency        :: Text
    , paymentMethodId :: PaymentMethodId
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''WithdrawalRequest


data WithdrawalResponse = WithdrawalResponse
    { wId       :: UUID
    , wAmount   :: Double
    , wCurrency :: Text
    , wPayoutAt :: UTCTime
    } deriving (Eq, Show)


instance FromJSON WithdrawalResponse where
  parseJSON = withObject "withdrawal response" $ \o -> WithdrawalResponse
    <$> o .: "id"
    <*> (read <$> o .: "amount")
    <*> o .: "currency"
    <*> o .: "payout_at"


data CoinbaseWithdrawalRequest = CoinbaseWithdrawalRequest
    { cAmount            :: Double
    , cCurrency          :: Text
    , cCoinbaseAccountId :: AccountId
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 1 } ''CoinbaseWithdrawalRequest


data CryptoWithdrawalRequest = CryptoWithdrawalRequest
    { crAmount        :: Double
    , crCurrency      :: Text
    , crCryptoAddress :: Text
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 2 } ''CryptoWithdrawalRequest


data CryptoWithdrawalResponse = CryptoWithdrawalResponse
    { cwId       :: UUID
    , cwAmount   :: Double
    , cwCurrency :: Text
    , cwFee      :: Double
    , cwSubtotal :: Double
    } deriving (Eq, Show)


instance FromJSON CryptoWithdrawalResponse where
  parseJSON = withObject "crypto withdrawal response" $ \o -> CryptoWithdrawalResponse
    <$> o .: "id"
    <*> (read <$> o .: "amount")
    <*> o .: "currency"
    <*> ((read <$> o .: "fee") <|> (o .: "fee"))
    <*> ((read <$> o .: "subtotal") <|> (o .: "subtotal"))
    


newtype WithdrawalFeeEstimateResponse = WithdrawalFeeEstimateResponse
    { wfFee :: Double} deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 2 } ''WithdrawalFeeEstimateResponse
