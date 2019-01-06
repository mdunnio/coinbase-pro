{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Request.Unauthenticated
   ( products
   , time
   , aggregateOrderBook
   , fullOrderBook
   ) where

import           Data.Proxy                                (Proxy (..))
import           Servant.API
import           Servant.Client

import           CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           CoinbasePro.MarketData.FullOrderBook      (FullOrderBook)
import           CoinbasePro.MarketData.Types              (AggregateBookLevel (..),
                                                            CBTime,
                                                            FullBookLevel (..),
                                                            Product)
import           CoinbasePro.Request                       (request)
import           CoinbasePro.Types                         (ProductId,
                                                            RequiredHeader,
                                                            UserAgent)


type API = "products" :> RequiredHeader "User-Agent" UserAgent :> Get '[JSON] [Product]
      :<|> "time" :> RequiredHeader "User-Agent" UserAgent :> Get '[JSON] CBTime
      :<|> "products" :> Capture "product" ProductId :> "book" :> QueryParam "level" AggregateBookLevel
      :> RequiredHeader "User-Agent" UserAgent :> Get '[JSON] AggregateOrderBook
      :<|> "products" :> Capture "product" ProductId :> "book" :> QueryParam "level" FullBookLevel
      :> RequiredHeader "User-Agent" UserAgent :> Get '[JSON] FullOrderBook


api :: Proxy API
api = Proxy


productsAPI :: UserAgent -> ClientM [Product]
timeAPI :: UserAgent -> ClientM CBTime
aggregateOrderBookAPI :: ProductId -> Maybe AggregateBookLevel -> UserAgent -> ClientM AggregateOrderBook
fullOrderBookAPI :: ProductId -> Maybe FullBookLevel -> UserAgent -> ClientM FullOrderBook
productsAPI :<|> timeAPI :<|> aggregateOrderBookAPI :<|> fullOrderBookAPI = client api


products :: IO [Product]
products = request productsAPI


time :: IO CBTime
time = request timeAPI


aggregateOrderBook :: ProductId -> Maybe AggregateBookLevel -> IO AggregateOrderBook
aggregateOrderBook prid = request . aggregateOrderBookAPI prid


fullOrderBook :: ProductId -> Maybe FullBookLevel -> IO FullOrderBook
fullOrderBook prid = request . fullOrderBookAPI prid
