{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Request.Unauthenticated
   ( products
   , time
   , aggregateOrderBook
   , fullOrderBook
   , trades
   ) where

import           Data.Proxy                                (Proxy (..))
import           Servant.API
import           Servant.Client

import           CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           CoinbasePro.MarketData.FullOrderBook      (FullOrderBook)
import           CoinbasePro.MarketData.Types              (AggregateBookLevel (..),
                                                            CBTime,
                                                            FullBookLevel (..),
                                                            Product, Trade)
import           CoinbasePro.Request                       (request)
import           CoinbasePro.Types                         (ProductId,
                                                            RequiredHeader,
                                                            UserAgent)


type UserAgentHeader = RequiredHeader "User-Agent" UserAgent


type API = "products" :> UserAgentHeader :> Get '[JSON] [Product]
      :<|> "time" :> UserAgentHeader :> Get '[JSON] CBTime
      :<|> "products" :> Capture "product" ProductId :> "book" :> QueryParam "level" AggregateBookLevel
      :> UserAgentHeader :> Get '[JSON] AggregateOrderBook
      :<|> "products" :> Capture "product" ProductId :> "book" :> QueryParam "level" FullBookLevel
      :> UserAgentHeader :> Get '[JSON] FullOrderBook
      :<|> "products" :> Capture "product" ProductId :> "trades" :> UserAgentHeader :> Get '[JSON] [Trade]


api :: Proxy API
api = Proxy


productsAPI :: UserAgent -> ClientM [Product]
timeAPI :: UserAgent -> ClientM CBTime
aggregateOrderBookAPI :: ProductId -> Maybe AggregateBookLevel -> UserAgent -> ClientM AggregateOrderBook
fullOrderBookAPI :: ProductId -> Maybe FullBookLevel -> UserAgent -> ClientM FullOrderBook
tradesAPI :: ProductId -> UserAgent -> ClientM [Trade]
productsAPI :<|> timeAPI :<|> aggregateOrderBookAPI :<|> fullOrderBookAPI :<|> tradesAPI = client api


products :: IO [Product]
products = request productsAPI


time :: IO CBTime
time = request timeAPI


aggregateOrderBook :: ProductId -> Maybe AggregateBookLevel -> IO AggregateOrderBook
aggregateOrderBook prid = request . aggregateOrderBookAPI prid


fullOrderBook :: ProductId -> Maybe FullBookLevel -> IO FullOrderBook
fullOrderBook prid = request . fullOrderBookAPI prid


trades :: ProductId -> IO [Trade]
trades = request . tradesAPI
