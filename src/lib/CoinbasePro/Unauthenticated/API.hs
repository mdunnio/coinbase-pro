{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Unauthenticated.API
    ( products
    , aggregateOrderBook
    , fullOrderBook
    , trades
    , candles
    , stats
    , time
    ) where

import           Data.Proxy                                (Proxy (..))
import           Data.Time.Clock                           (UTCTime)
import           Servant.API                               ((:<|>) (..), (:>),
                                                            Capture, Get, JSON,
                                                            QueryParam,
                                                            QueryParam',
                                                            Required)
import           Servant.Client                            (ClientM, client)

import           CoinbasePro.Headers                       (UserAgent,
                                                            UserAgentHeader)
import           CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           CoinbasePro.MarketData.FullOrderBook      (FullOrderBook)
import           CoinbasePro.MarketData.Types              (AggregateBookLevel (..),
                                                            CBTime,
                                                            FullBookLevel (..),
                                                            Product, Trade)
import           CoinbasePro.Types                         (Candle,
                                                            CandleGranularity,
                                                            ProductId,
                                                            TwentyFourHourStats)

type Products = "products" :> UserAgentHeader :> Get '[JSON] [Product]

type ProductAggregateOrderBook = "products"
                               :> Capture "product" ProductId
                               :> "book"
                               :> QueryParam "level" AggregateBookLevel
                               :> UserAgentHeader
                               :> Get '[JSON] AggregateOrderBook

type ProductFullOrderBook = "products"
                          :> Capture "product" ProductId
                          :> "book"
                          :> QueryParam "level" FullBookLevel
                          :> UserAgentHeader
                          :> Get '[JSON] FullOrderBook

type Trades = "products"
            :> Capture "product" ProductId
            :> "trades"
            :> UserAgentHeader
            :> Get '[JSON] [Trade]


type Candles = "products"
             :> Capture "product" ProductId
             :> "candles"
             :> QueryParam "start" UTCTime
             :> QueryParam "end" UTCTime
             :> QueryParam' '[Required] "granularity" CandleGranularity
             :> UserAgentHeader
             :> Get '[JSON] [Candle]

type Stats = "products"
            :> Capture "product" ProductId
            :> "stats"
            :> UserAgentHeader
            :> Get '[JSON] TwentyFourHourStats

type Time = "time"
          :> UserAgentHeader
          :> Get '[JSON] CBTime

type API =    Products
         :<|> ProductAggregateOrderBook
         :<|> ProductFullOrderBook
         :<|> Trades
         :<|> Candles
         :<|> Stats
         :<|> Time


api :: Proxy API
api = Proxy


products :: UserAgent -> ClientM [Product]
aggregateOrderBook :: ProductId -> Maybe AggregateBookLevel -> UserAgent -> ClientM AggregateOrderBook
fullOrderBook :: ProductId -> Maybe FullBookLevel -> UserAgent -> ClientM FullOrderBook
trades :: ProductId -> UserAgent -> ClientM [Trade]
candles :: ProductId -> Maybe UTCTime -> Maybe UTCTime -> CandleGranularity -> UserAgent -> ClientM [Candle]
stats :: ProductId -> UserAgent -> ClientM TwentyFourHourStats
time :: UserAgent -> ClientM CBTime
products :<|> aggregateOrderBook :<|> fullOrderBook :<|> trades :<|> candles :<|> stats :<|> time = client api
