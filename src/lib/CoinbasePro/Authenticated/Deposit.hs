{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Deposit
    ( Deposit (..)
    , DepositRequest (..)
    , DepositResponse (..)
    , PaymentMethodId
    ) where

import           Data.Aeson                         (FromJSON, ToJSON,
                                                     parseJSON, withObject,
                                                     (.:), (.:?))
import           Data.Aeson.Casing                  (snakeCase)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           Data.Text                          (Text, pack, toLower)
import           Data.Time.Clock                    (UTCTime)
import           Data.UUID                          (UUID)
import           Servant.API

import           CoinbasePro.Authenticated.Accounts (AccountId)
import           CoinbasePro.Types                  (CreatedAt, UserId)


data DepositDetails = DepositDetails
    { cryptoAddress         :: Text
    , destinationTag        :: Text
    , coinbaseAccountId     :: AccountId
    , destinationTagName    :: Text
    , coinbaseTransactionId :: Text
    , cryptoTransactionHash :: Text
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''DepositDetails


newtype PaymentMethodId = PaymentMethodId UUID
  deriving (Eq)


instance Show PaymentMethodId where
  show (PaymentMethodId p) = show p


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''PaymentMethodId


instance ToHttpApiData PaymentMethodId where
    toUrlPiece   = toLower . pack . show
    toQueryParam = toLower . pack . show


newtype UserNonce = UserNonce Int
  deriving (Eq, Show, ToJSON, FromJSON)


data Deposit = Deposit
    { dId          :: PaymentMethodId
    , dType        :: Text
    , dCreatedAt   :: CreatedAt
    , dCompletedAt :: UTCTime
    , dCanceledAt  :: Maybe UTCTime
    , dProcessedAt :: Maybe UTCTime
    , dAccountId   :: AccountId
    , dUserId      :: UserId
    , dUserNonce   :: Maybe UserNonce
    , dAmount      :: Double
    , dDetails     :: Maybe DepositDetails
    } deriving (Eq, Show)


instance FromJSON Deposit where
  parseJSON = withObject "deposit" $ \o -> Deposit
    <$> (o .: "id")
    <*> (o .: "type")
    <*> (o .: "created_at")
    <*> (o .: "completed_at")
    <*> (o .:? "canceled_at")
    <*> (o .:? "processed_at")
    <*> (o .: "account_id")
    <*> (o .: "user_id")
    <*> (o .:? "user_nonce")
    <*> (read <$> o .: "amount")
    <*> (o .:? "deposit_details")


data DepositRequest = DepositRequest
    { amount          :: Double
    , currency        :: Text
    , paymentMethodId :: PaymentMethodId
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''DepositRequest


data DepositResponse = DepositResponse
    { rId       :: UUID
    , rAmount   :: Double
    , rCurrency :: Text
    , rPayoutAt :: UTCTime
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 1 } ''DepositResponse
