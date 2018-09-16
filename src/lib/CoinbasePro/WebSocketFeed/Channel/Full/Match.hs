{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Full.Match
    ( Match (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:))
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (OrderId, Price, ProductId, Sequence, Side,
                                    Size)


data Match = Match
    { tradeId      :: Int
    , sequence     :: Sequence
    , makerOrderId :: OrderId
    , takerOrderId :: OrderId
    , time         :: UTCTime
    , productId    :: ProductId
    , size         :: Size
    , price        :: Price
    , side         :: Side
    } deriving (Eq, Ord, Show)


instance FromJSON Match where
    parseJSON = withObject "match" $ \o -> do
        trid <- o .: "trade_id"
        sq   <- o .: "sequence"
        moid <- o .: "maker_order_id"
        toid <- o .: "taker_order_id"
        ti   <- o .: "time"
        prid <- o .: "product_id"
        sz   <- o .: "size"
        p    <- o .: "price"
        s    <- o .: "side"
        return $ Match trid sq moid toid ti prid (read sz) (read p) s
