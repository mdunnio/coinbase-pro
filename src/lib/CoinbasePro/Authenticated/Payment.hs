{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module CoinbasePro.Authenticated.Payment
  ( PaymentMethodId (..)
  , PaymentMethod (..)
  ) where

import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Text         (Text, toLower, unpack)
import           Servant.API


newtype PaymentMethodId = PaymentMethodId Text
  deriving Eq


instance Show PaymentMethodId where
  show (PaymentMethodId p) = unpack p


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''PaymentMethodId


instance ToHttpApiData PaymentMethodId where
    toUrlPiece   (PaymentMethodId p) = toLower p
    toQueryParam (PaymentMethodId p) = toLower p


data LimitTotal = LimitTotal
    { ltAmount   :: Double
    , ltCurrency :: Text
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 2} ''LimitTotal


data LimitRemaining = LimitRemaining
    { lrAmount   :: Double
    , lrCurrency :: Text
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 2} ''LimitRemaining


data Limit = Limit
    { periodInDays :: Int
    , total        :: LimitTotal
    , remaining    :: LimitRemaining
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''Limit


data PaymentMethod = PaymentMethod
    { pmId               :: PaymentMethodId
    , pmType             :: Text
    , pmName             :: Text
    , pmCurrency         :: Text
    , pmPrimaryBuy       :: Bool
    , pmPrimarySell      :: Bool
    , pmAllowBuy         :: Bool
    , pmAllowSell        :: Bool
    , pmAllowDeposit     :: Bool
    , pmAllowWithdraw    :: Bool
    , pmBuyLimits        :: Maybe [Limit]
    , pmInstantBuyLimits :: Maybe [Limit]
    , pmSellLimits       :: Maybe [Limit]
    , pmDepositLimits    :: Maybe [Limit]
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 2 } ''PaymentMethod
