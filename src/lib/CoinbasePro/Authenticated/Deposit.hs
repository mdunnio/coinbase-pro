{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Deposit
    ( Deposit (..)
    ) where

import           Data.Aeson                         (FromJSON, ToJSON,
                                                     parseJSON, withObject,
                                                     (.:), (.:?))
import           Data.Aeson.Casing                  (snakeCase)
import           Data.Aeson.TH                      (defaultOptions, deriveJSON,
                                                     fieldLabelModifier)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import           Data.UUID                          (UUID)

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


newtype UserNonce = UserNonce Int
  deriving (Eq, Show, ToJSON, FromJSON)


data Deposit = Deposit
    { dId          :: UUID
    , dType        :: Text
    , dCreatedAt   :: CreatedAt
    , dCompletedAt :: UTCTime
    , dCanceledAt  :: Maybe UTCTime
    , dProcessedAt :: Maybe UTCTime
    , dAccountId   :: AccountId
    , dUserId      :: UserId
    , dUserNonce   :: Maybe Int
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
