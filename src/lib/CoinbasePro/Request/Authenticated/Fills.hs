{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.Request.Authenticated.Fills
    ( Fill (..)
    , TradeId
    , Liquidity (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, withText, (.:))

import           CoinbasePro.Types (CreatedAt (..), OrderId, Price, ProductId,
                                    Side, Size)


newtype TradeId = TradeId Int
    deriving (Eq, Show)


data Liquidity = Maker | Taker
    deriving (Eq, Show)


instance FromJSON Liquidity where
    parseJSON = withText "liquidity" $ \t ->
      case t of
        "M" -> return Maker
        "T" -> return Taker
        _   -> fail "parse error"


data Fill = Fill
    { tradeId   :: TradeId
    , productId :: ProductId
    , side      :: Side
    , price     :: Price
    , size      :: Size
    , orderId   :: OrderId
    , createdAt :: CreatedAt
    , liquidiy  :: Liquidity
    , fee       :: Double
    , settled   :: Bool
    } deriving (Eq, Show)


instance FromJSON Fill where
    parseJSON = withObject "fill" $ \o -> Fill
        <$> (TradeId <$> o .: "trade_id")
        <*> o .: "product_id"
        <*> o .: "side"
        <*> o .: "price"
        <*> o .: "size"
        <*> o .: "order_id"
        <*> (CreatedAt <$> o .: "created_at")
        <*> (o .: "liquidity")
        <*> (read <$> o .: "fee")
        <*> o .: "settled"
