{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Accounts
    ( Account (..)
    , AccountId (..)
    , Currency (..)
    , Balance (..)
    , ProfileId (..)
    , Fees (..)
    , TrailingVolume (..)
    , AccountHistory (..)
    , Hold (..)
    ) where

import           Data.Aeson        (FromJSON (..), ToJSON, withObject, withText,
                                    (.:), (.:?))
import qualified Data.Aeson        as A
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (constructorTagModifier, defaultOptions,
                                    deriveJSON, fieldLabelModifier)
import qualified Data.Char         as Char
import           Data.Text         (Text, pack, unpack)
import           Data.Time.Clock   (UTCTime)
import           Text.Printf       (printf)
import           Web.HttpApiData   (ToHttpApiData (..))

import           CoinbasePro.Types (CreatedAt (..), OrderId, ProductId,
                                    TradeId (..), Volume (..))


newtype AccountId = AccountId Text
    deriving (Eq, Show)


deriveJSON defaultOptions
    { fieldLabelModifier = snakeCase
    } ''AccountId


instance ToHttpApiData AccountId where
    toUrlPiece (AccountId aid)   = aid
    toQueryParam (AccountId aid) = aid


newtype Currency = Currency Text
    deriving (Eq, Show)


newtype Balance = Balance Double
    deriving (Eq, Show)


instance ToJSON Balance where
    toJSON (Balance s) = A.String . pack $ printf "%.16f" s


instance FromJSON Balance where
    parseJSON = withText "balance" $ \t ->
      return . Balance . read $ unpack t


newtype ProfileId = ProfileId Text
    deriving (Eq, Show)


data Account = Account
    { accountId :: AccountId
    , currency  :: Currency
    , balance   :: Balance
    , available :: Balance
    , hold      :: Balance
    , profileId :: ProfileId
    } deriving (Eq, Show)


instance FromJSON Account where
    parseJSON = withObject "account" $ \o -> Account
        <$> (AccountId <$> o .: "id")
        <*> (Currency <$> o .: "currency")
        <*> (Balance . read <$> o .: "balance")
        <*> (Balance . read <$> o .: "available")
        <*> (Balance . read <$> o .: "hold")
        <*> (ProfileId <$> o .: "profile_id")


newtype FeeRate = FeeRate { unFeeRate :: Double }
    deriving (Eq, Show, FromJSON, ToJSON)


data Fees = Fees
    { makerFeeRate :: FeeRate
    , takerFeeRate :: FeeRate
    , usdVolume    :: Volume
    } deriving (Eq, Show)


instance FromJSON Fees where
    parseJSON = withObject "fees" $ \o -> Fees
        <$> (FeeRate . read <$> o .: "maker_fee_rate")
        <*> (FeeRate . read <$> o .: "taker_fee_rate")
        <*> (Volume . read <$> o .: "usd_volume")


data TrailingVolume = TrailingVolume
    { productId      :: ProductId
    , exchangeVolume :: Volume
    , volume         :: Volume
    , recordedAt     :: UTCTime
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''TrailingVolume


data AccountHistoryType = Transfer | Match | Fee | Rebate | Conversion
    deriving (Eq, Show)


deriveJSON defaultOptions { constructorTagModifier = fmap Char.toLower } ''AccountHistoryType


data Details = Details
    { dOrderId   :: Maybe OrderId
    , dTradeId   :: Maybe TradeId
    , dProductId :: Maybe ProductId
    } deriving (Eq, Show)


instance FromJSON Details where
    parseJSON = withObject "details" $ \o -> Details
        <$> (o .:? "order_id")
        <*> (fmap (TradeId . read) <$> o .:? "trade_id")
        <*> (o .:? "product_id")


data AccountHistory = AccountHistory
    { hAccountId :: AccountId
    , hCreatedAt :: CreatedAt
    , hAmount    :: Double -- TODO: Give this a newtype
    , hBalance   :: Balance
    , hType      :: AccountHistoryType
    , hDetails   :: Maybe Details
    } deriving (Eq, Show)


instance FromJSON AccountHistory where
    parseJSON = withObject "account_history" $ \o -> AccountHistory
        <$> (AccountId <$> o .: "id")
        <*> (CreatedAt <$> o .: "created_at")
        <*> (read <$> o .: "amount")
        <*> (Balance . read <$> o .: "balance")
        <*> (o .: "type")
        <*> (o .: "details")


newtype HoldId = HoldId Text
  deriving (Eq, Show, ToJSON, FromJSON)


data HoldType = Order | HoldTransfer
  deriving (Eq, Show)


newtype HoldRef = HoldRef Text
  deriving (Eq, Show, ToJSON, FromJSON)


deriveJSON defaultOptions { constructorTagModifier = fmap Char.toLower } ''HoldType


data Hold = Hold
    { holdId        :: HoldId
    , holdAccountId :: AccountId
    , holdCreatedAt :: CreatedAt
    , holdUpdatedAt :: CreatedAt
    , holdAmount    :: Double -- TODO: Give this a newtype
    , holdType      :: HoldType
    , holdRef       :: HoldRef
    } deriving (Eq, Show)


deriveJSON defaultOptions { constructorTagModifier = fmap Char.toLower . drop 4, fieldLabelModifier = snakeCase } ''Hold
