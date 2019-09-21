{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Unauthenticated
   ( products
   , time
   , aggregateOrderBook
   , fullOrderBook
   , trades
   , candles
   , stats
   ) where

import           Data.Time.Clock                           (UTCTime)

import           CoinbasePro.Headers                       (userAgent)
import           CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           CoinbasePro.MarketData.FullOrderBook      (FullOrderBook)
import           CoinbasePro.MarketData.Types              (AggregateBookLevel (..),
                                                            CBTime,
                                                            FullBookLevel (..),
                                                            Product, Trade)
import           CoinbasePro.Request                       (run)
import           CoinbasePro.Types                         (Candle,
                                                            CandleGranularity,
                                                            ProductId,
                                                            TwentyFourHourStats)
import qualified CoinbasePro.Unauthenticated.API           as API


-- | https://docs.pro.coinbase.com/#get-products
products :: IO [Product]
products = run $ API.products userAgent


-- | https://docs.pro.coinbase.com/#time
time :: IO CBTime
time = run $ API.time userAgent


-- | https://docs.pro.coinbase.com/#get-product-order-book
aggregateOrderBook :: ProductId -> Maybe AggregateBookLevel -> IO AggregateOrderBook
aggregateOrderBook prid agg = run $ API.aggregateOrderBook prid agg userAgent


-- | https://docs.pro.coinbase.com/#get-product-order-book
fullOrderBook :: ProductId -> IO FullOrderBook
fullOrderBook prid = run $ API.fullOrderBook prid (Just FullBookLevel) userAgent


-- | https://docs.pro.coinbase.com/#get-trades
trades :: ProductId -> IO [Trade]
trades prid = run $ API.trades prid userAgent


-- | https://docs.pro.coinbase.com/#get-historic-rates
candles :: ProductId -> Maybe UTCTime -> Maybe UTCTime -> CandleGranularity -> IO [Candle]
candles prid start end cg = run $ API.candles prid start end cg userAgent


-- | https://docs.pro.coinbase.com/#get-24hr-stats
stats :: ProductId -> IO TwentyFourHourStats
stats prid = run $ API.stats prid userAgent
