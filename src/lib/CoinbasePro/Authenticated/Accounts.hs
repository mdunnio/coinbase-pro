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
    ) where

import           Data.Aeson        (FromJSON (..), ToJSON, withObject, (.:))
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Text         (Text)
import           Data.Time.Clock   (UTCTime)
import           Web.HttpApiData   (ToHttpApiData (..))

import           CoinbasePro.Types (ProductId, Volume (..))


newtype AccountId = AccountId Text
    deriving (Eq, Show)


instance ToHttpApiData AccountId where
    toUrlPiece (AccountId aid)   = aid
    toQueryParam (AccountId aid) = aid


newtype Currency = Currency Text
    deriving (Eq, Show)


newtype Balance = Balance Double
    deriving (Eq, Show)


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
