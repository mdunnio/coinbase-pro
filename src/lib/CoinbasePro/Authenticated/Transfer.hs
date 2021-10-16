{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module CoinbasePro.Authenticated.Transfer
    ( TransferType (..)
    , Transfer (..)
    , TransferDetails (..)
    , WithdrawalTransfer (..)
    , DepositTransfer (..)
    ) where

import           Data.Aeson                           (FromJSON (..), ToJSON,
                                                       Value (..), parseJSON,
                                                       withObject, (.:), (.:?))
import           Data.Text                            (Text, pack)
import           Data.Time.Clock                      (UTCTime)
import           Servant.API

import           CoinbasePro.Authenticated.Accounts   (AccountId)
import           CoinbasePro.Authenticated.Deposit    (DepositDetails)
import           CoinbasePro.Authenticated.Withdrawal (WithdrawalDetails)
import           CoinbasePro.Types                    (CreatedAt, UserId)


data TransferType = WithdrawTransferType | DepositTransferType


instance Show TransferType where
  show WithdrawTransferType = "withdraw"
  show DepositTransferType  = "deposit"


instance ToHttpApiData TransferType where
    toUrlPiece   = pack . show
    toQueryParam = pack . show


newtype UserNonce = UserNonce Int
  deriving (Eq, Show, ToJSON, FromJSON)


data TransferDetails = TransferDetails
    { tId          :: Text
    , tType        :: Text
    , tCreatedAt   :: CreatedAt
    , tCompletedAt :: Maybe UTCTime
    , tCanceledAt  :: Maybe UTCTime
    , tProcessedAt :: Maybe UTCTime
    , tAccountId   :: AccountId
    , tUserId      :: UserId
    , tUserNonce   :: Maybe UserNonce
    , tAmount      :: Double
    } deriving Show


instance FromJSON TransferDetails where
  parseJSON = withObject "transfer" $ \o -> TransferDetails
    <$> (o .: "id")
    <*> (o .: "type")
    <*> (o .: "created_at")
    <*> (o .:? "completed_at")
    <*> (o .:? "canceled_at")
    <*> (o .:? "processed_at")
    <*> (o .: "account_id")
    <*> (o .: "user_id")
    <*> (o .:? "user_nonce")
    <*> (read <$> o .: "amount")


data WithdrawalTransfer = WithdrawalTransfer
    { wTransfer :: TransferDetails
    , wDetails  :: WithdrawalDetails
    } deriving Show


instance FromJSON WithdrawalTransfer where
  parseJSON = withObject "withdrawal transfer" $ \o -> WithdrawalTransfer
    <$> parseJSON (Object o)
    <*> o .: "details"


data DepositTransfer = DepositTransfer
    { dTransfer :: TransferDetails
    , dDetails  :: DepositDetails
    } deriving Show


instance FromJSON DepositTransfer where
  parseJSON = withObject "deposit transfer" $ \o -> DepositTransfer
    <$> parseJSON (Object o)
    <*> o .: "details"


data Transfer = Withdrawal WithdrawalTransfer | Deposit DepositTransfer
  deriving Show


instance FromJSON Transfer where
  parseJSON = withObject "transfer" $ \o -> do
    t <- String <$> o .: "type"
    case t of
      "withdraw" -> Withdrawal <$> parseJSON (Object o)
      "deposit"  -> Deposit <$> parseJSON (Object o)
      _          -> fail "Unable to parse transfer"
