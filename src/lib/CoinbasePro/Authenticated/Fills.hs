{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.Authenticated.Fills
    ( Fill (..)
    , TradeId
    , Liquidity (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, withText, (.:))

import           CoinbasePro.Types (CreatedAt (..), OrderId, Price, ProductId,
                                    Side, Size, TradeId (..))


data Liquidity = Maker | Taker
    deriving (Eq, Show)


instance FromJSON Liquidity where
    parseJSON = withText "liquidity" $
      \case
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
    , liquidity :: Liquidity
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
