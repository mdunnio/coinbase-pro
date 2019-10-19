{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Unauthenticated.API
    ( products
    , aggregateOrderBook
    , fullOrderBook
    , trades
    , candles
    , stats
    , currencies
    , time
    ) where

import           Data.Proxy                                (Proxy (..))
import           Data.Time.Clock                           (UTCTime)
import           Servant.API                               ((:<|>) (..), (:>),
                                                            Capture, QueryParam,
                                                            QueryParam',
                                                            Required)
import           Servant.Client                            (client)

import           CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           CoinbasePro.MarketData.FullOrderBook      (FullOrderBook)
import           CoinbasePro.MarketData.Types              (AggregateBookLevel (..),
                                                            CBTime,
                                                            FullBookLevel (..),
                                                            Product, Trade)
import           CoinbasePro.Request                       (CBGet, CBRequest)
import           CoinbasePro.Types                         (Candle,
                                                            CandleGranularity,
                                                            Currency, ProductId,
                                                            TwentyFourHourStats)


type Products = "products" :> CBGet [Product]

type ProductAggregateOrderBook = "products"
                               :> Capture "product" ProductId
                               :> "book"
                               :> QueryParam "level" AggregateBookLevel
                               :> CBGet AggregateOrderBook

type ProductFullOrderBook = "products"
                          :> Capture "product" ProductId
                          :> "book"
                          :> QueryParam "level" FullBookLevel
                          :> CBGet FullOrderBook

type Trades = "products"
            :> Capture "product" ProductId
            :> "trades"
            :> CBGet [Trade]

type Candles = "products"
             :> Capture "product" ProductId
             :> "candles"
             :> QueryParam "start" UTCTime
             :> QueryParam "end" UTCTime
             :> QueryParam' '[Required] "granularity" CandleGranularity
             :> CBGet [Candle]

type Stats = "products"
            :> Capture "product" ProductId
            :> "stats"
            :> CBGet TwentyFourHourStats

type Currencies = "currencies"
                :> CBGet [Currency]

type Time = "time" :> CBGet CBTime

type API =    Products
         :<|> ProductAggregateOrderBook
         :<|> ProductFullOrderBook
         :<|> Trades
         :<|> Candles
         :<|> Stats
         :<|> Currencies
         :<|> Time


api :: Proxy API
api = Proxy


products :: CBRequest [Product]
aggregateOrderBook :: ProductId -> Maybe AggregateBookLevel -> CBRequest AggregateOrderBook
fullOrderBook :: ProductId -> Maybe FullBookLevel -> CBRequest FullOrderBook
trades :: ProductId -> CBRequest [Trade]
candles :: ProductId -> Maybe UTCTime -> Maybe UTCTime -> CandleGranularity -> CBRequest [Candle]
stats :: ProductId -> CBRequest TwentyFourHourStats
currencies :: CBRequest [Currency]
time :: CBRequest CBTime
products :<|> aggregateOrderBook :<|> fullOrderBook :<|> trades :<|> candles :<|> stats :<|> currencies :<|> time = client api
