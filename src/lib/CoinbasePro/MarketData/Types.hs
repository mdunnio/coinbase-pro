{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.MarketData.Types
    ( Product (..)
    , CBTime (..)
    , AggregateBookLevel (..)
    , FullBookLevel (..)
    , Trade (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:))
import           Data.Aeson.Casing (snakeCase)
import           Data.Aeson.TH     (defaultOptions, deriveJSON,
                                    fieldLabelModifier)
import           Data.Text         (Text, pack)
import           Data.Time.Clock   (UTCTime)
import           Web.HttpApiData   (ToHttpApiData (..))

import           CoinbasePro.Types (Price, ProductId, Side, Size, TradeId)


data Product = Product
    { productId      :: ProductId
    , baseCurrency   :: Text
    , quoteCurrency  :: Text
    , baseIncrement  :: Double
    , quoteIncrement :: Double
    , minMarketFunds :: Double
    , displayName    :: Text
    } deriving (Eq, Show)


instance FromJSON Product where
    parseJSON = withObject "product" $ \o -> Product
        <$> o .: "id"
        <*> o .: "base_currency"
        <*> o .: "quote_currency"
        <*> (read <$> o .: "base_increment")
        <*> (read <$> o .: "quote_increment")
        <*> (read <$> o .: "min_market_funds")
        <*> o .: "display_name"


instance ToHttpApiData Product where
    toUrlPiece   = toUrlPiece . productId
    toQueryParam = toQueryParam . productId


newtype CBTime = CBTime { unCBTime :: UTCTime } deriving (Eq, Show, Ord)


instance FromJSON CBTime where
    parseJSON = withObject "time" $ \o ->
      CBTime <$> o .: "iso"


data AggregateBookLevel = Best | TopFifty
    deriving (Eq, Show)


instance ToHttpApiData AggregateBookLevel where
    toUrlPiece = pack . show . aggregateBookLevel
    toQueryParam = pack . show . aggregateBookLevel


aggregateBookLevel :: AggregateBookLevel -> Int
aggregateBookLevel Best     = 1
aggregateBookLevel TopFifty = 2


data FullBookLevel = FullBookLevel


instance ToHttpApiData FullBookLevel where
    toUrlPiece = pack . show . fullBookLevel
    toQueryParam = pack . show . fullBookLevel


fullBookLevel :: FullBookLevel -> Int
fullBookLevel FullBookLevel = 3


data Trade = Trade
    { time    :: UTCTime
    , tradeId :: TradeId
    , price   :: Price
    , size    :: Size
    , side    :: Side
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''Trade
