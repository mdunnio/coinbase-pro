{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Request.Unauthenticated
   ( products
   , time
   , aggregateOrderBook
   , fullOrderBook
   , trades
   , candles
   ) where

import           Data.Proxy                                (Proxy (..))
import           Data.Time.Clock                           (UTCTime)
import           Servant.API
import           Servant.Client

import           CoinbasePro.Headers                       (UserAgent,
                                                            UserAgentHeader)
import           CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           CoinbasePro.MarketData.FullOrderBook      (FullOrderBook)
import           CoinbasePro.MarketData.Types              (AggregateBookLevel (..),
                                                            CBTime,
                                                            FullBookLevel (..),
                                                            Product, Trade)
import           CoinbasePro.Request                       (request)
import           CoinbasePro.Types                         (Candle,
                                                            CandleGranularity,
                                                            ProductId)


type API = "products" :> UserAgentHeader :> Get '[JSON] [Product]
      :<|> "time" :> UserAgentHeader :> Get '[JSON] CBTime
      :<|> "products" :> Capture "product" ProductId :> "book" :> QueryParam "level" AggregateBookLevel
      :> UserAgentHeader :> Get '[JSON] AggregateOrderBook
      :<|> "products" :> Capture "product" ProductId :> "book" :> QueryParam "level" FullBookLevel
      :> UserAgentHeader :> Get '[JSON] FullOrderBook
      :<|> "products" :> Capture "product" ProductId :> "trades" :> UserAgentHeader :> Get '[JSON] [Trade]
      :<|> "products" :> Capture "product" ProductId :> "candles" :> QueryParam "start" UTCTime
      :> QueryParam "end" UTCTime :> QueryParam' '[Required] "granularity" CandleGranularity
      :> UserAgentHeader :> Get '[JSON] [Candle]


api :: Proxy API
api = Proxy


productsAPI :: UserAgent -> ClientM [Product]
timeAPI :: UserAgent -> ClientM CBTime
aggregateOrderBookAPI :: ProductId -> Maybe AggregateBookLevel -> UserAgent -> ClientM AggregateOrderBook
fullOrderBookAPI :: ProductId -> Maybe FullBookLevel -> UserAgent -> ClientM FullOrderBook
tradesAPI :: ProductId -> UserAgent -> ClientM [Trade]
candlesAPI :: ProductId -> Maybe UTCTime -> Maybe UTCTime -> CandleGranularity -> UserAgent -> ClientM [Candle]
productsAPI :<|> timeAPI :<|> aggregateOrderBookAPI :<|> fullOrderBookAPI :<|> tradesAPI :<|> candlesAPI = client api


-- | https://docs.pro.coinbase.com/#get-products
products :: IO [Product]
products = request productsAPI


-- | https://docs.pro.coinbase.com/#time
time :: IO CBTime
time = request timeAPI


-- | https://docs.pro.coinbase.com/#get-product-order-book
aggregateOrderBook :: ProductId -> Maybe AggregateBookLevel -> IO AggregateOrderBook
aggregateOrderBook prid = request . aggregateOrderBookAPI prid


-- | https://docs.pro.coinbase.com/#get-product-order-book
fullOrderBook :: ProductId -> Maybe FullBookLevel -> IO FullOrderBook
fullOrderBook prid = request . fullOrderBookAPI prid


-- | https://docs.pro.coinbase.com/#get-trades
trades :: ProductId -> IO [Trade]
trades = request . tradesAPI


-- | https://docs.pro.coinbase.com/#get-historic-rates
candles :: ProductId -> Maybe UTCTime -> Maybe UTCTime -> CandleGranularity -> IO [Candle]
candles prid start end = request . candlesAPI prid start end
