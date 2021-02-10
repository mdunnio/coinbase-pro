{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.Authenticated.Withdrawal
    ( WithdrawalDetails (..)
    ) where

import           Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import           Data.Text  (Text)


data WithdrawalDetails = WithdrawalDetails
    { destinationTag        :: Maybe Text
    , sentToAddress         :: Maybe Text
    , coinbaseAccountId     :: Text
    , destinationTagName    :: Maybe Text
    , coinbaseWithdrawalId  :: Maybe Text
    , coinbaseTransactionId :: Text
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
    <*> o .: "coinbase_transaction_id"
    <*> o .: "coinbase_payment_method_id"
    <*> (maybe Nothing read <$> o .:? "fee")
    <*> (maybe Nothing read <$> o .:? "subtotal")
