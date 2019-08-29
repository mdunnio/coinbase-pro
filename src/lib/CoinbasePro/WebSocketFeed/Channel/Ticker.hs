{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CoinbasePro.WebSocketFeed.Channel.Ticker
    ( Ticker (..)
    ) where

import           Data.Aeson.TH     (defaultOptions, deriveJSON)
import           Data.Time.Clock   (UTCTime)

import           CoinbasePro.Types (Price, ProductId, Sequence, Side, Size)


type Volume24h = Double
type Volume30d = Double


data Ticker = Ticker
    { sequence  :: Sequence
    , productId :: ProductId
    , price     :: Price
    , openPrice :: Price -- last 24h
    , volume24h :: Volume24h -- last 24h
    , lowPrice  :: Price -- last 24h
    , highPrice :: Price -- last 24h
    , volume30d :: Volume30d
    , bestBid   :: Price
    , bestAsk   :: Price
    , side      :: Maybe Side
    , time      :: Maybe UTCTime
    , tradeId   :: Maybe Int
    , lastSize  :: Maybe Size
    } deriving (Eq, Ord, Show)


deriveJSON defaultOptions ''Ticker
