{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.Authenticated.Accounts
    ( Account (..)
    , AccountId (..)
    , Currency (..)
    , Balance (..)
    , ProfileId (..)
    , TrailingVolume (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:))
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Text         (Text)
import           Data.Time.Clock   (UTCTime)
import           Web.HttpApiData   (ToHttpApiData (..))

import           CoinbasePro.Types (ProductId, Volume)


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


data TrailingVolume = TrailingVolume
    { productId      :: ProductId
    , exchangeVolume :: Volume
    , volume         :: Volume
    , recordedAt     :: UTCTime
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''TrailingVolume
