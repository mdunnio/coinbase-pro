{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Ticker
    ( Ticker (..)
    ) where

import           Data.Aeson        (FromJSON (..), withObject, (.:), (.:?))
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (Price, ProductId, Sequence, Side, Size)

data Ticker = Ticker
    { sequence  :: Sequence
    , productId :: ProductId
    , price     :: Price
    , openPrice :: Price -- last 24h
    , volume24h :: Double -- last 24h
    , lowPrice  :: Price -- last 24h
    , highPrice :: Price -- last 24h
    , volume30d :: Double
    , bid       :: Price
    , ask       :: Price
    , side      :: Maybe Side
    , time      :: Maybe UTCTime
    , tradeId   :: Maybe Int
    , lastSize  :: Maybe Size
    } deriving (Eq, Ord, Show)


instance FromJSON Ticker where
    parseJSON = withObject "ticker" $ \o -> do
        sq   <- o .: "sequence"
        prid <- o .: "product_id"
        p    <- o .: "price"
        o24h <- o .: "open_24h"
        v24h <- o .: "volume_24h"
        l24h <- o .: "low_24h"
        h24h <- o .: "high_24h"
        v30d <- o .: "volume_30d"
        b    <- o .: "best_bid"
        a    <- o .: "best_ask"
        s    <- o .:? "side"
        ti   <- o .:? "time"
        trid <- o .:? "trade_id"
        ls   <- o .:? "last_size"
        return $
            Ticker sq prid (read p) (read o24h) (read v24h) (read l24h)
            (read h24h) (read v30d) (read b) (read a) s ti trid (read <$> ls)
