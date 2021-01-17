{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Limits
  ( Limits (..)
  ) where

import           Data.Aeson        (FromJSON, ToJSON, parseJSON, withText)
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Map.Strict   (Map)
import           Data.Text         (Text, unpack)

import           CoinbasePro.Types (ProductId)


newtype LimitCurrency = LimitCurrency Text
  deriving (Eq, Show, ToJSON, FromJSON)


newtype Max = Max Double
  deriving (Eq, Show, ToJSON, FromJSON)


newtype Remaining = Remaining Double
  deriving (Eq, Show, ToJSON, FromJSON)


newtype PeriodInDays = PeriodInDays Int
  deriving (Eq, Show, ToJSON)


instance FromJSON PeriodInDays where
  parseJSON = withText "period_in_days" $ \t ->
    return . PeriodInDays . read $ unpack t


data Limit = Limit
  { max          :: Max
  , remaining    :: Remaining
  , periodInDays :: Maybe PeriodInDays
  } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''Limit


type LimitMap = Map ProductId Limit


data TransferLimits = TransferLimits
  { ach                  :: Maybe LimitMap
  , achNoBalance         :: Maybe LimitMap
  , creditDebitCard      :: Maybe LimitMap
  , achCurm              :: Maybe LimitMap
  , secure3dBuy          :: Maybe LimitMap
  , exchangeWithdraw     :: Maybe LimitMap
  , exchangeAch          :: Maybe LimitMap
  , paypalWithdrawal     :: Maybe LimitMap
  , instantAchWithdrawal :: Maybe LimitMap
  , instantBuy           :: Maybe LimitMap
  , buy                  :: Maybe LimitMap
  , sell                 :: Maybe LimitMap
  } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''TransferLimits


data Limits = Limits
    { limitCurrency  :: LimitCurrency
    , transferLimits :: TransferLimits
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''Limits
