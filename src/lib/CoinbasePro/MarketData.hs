{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.MarketData
    ( getProducts
    , getTime
    , getTopOfBook
    , getTopFiftyOfBook
    , getFullOrderBook
    ) where

import           Data.Monoid                               ((<>))
import           Data.Text                                 (Text)

import           CoinbasePro.MarketData.AggregateOrderBook (AggregateOrderBook)
import           CoinbasePro.MarketData.OrderBook          (OrderBook)
import           CoinbasePro.MarketData.Types              (CBTime, Product)
import           CoinbasePro.Request                       (CBT, request)


getProducts :: CBT IO [Product]
getProducts = request "/products"


getTime :: CBT IO CBTime
getTime = request "/time"


getTopOfBook :: Text -> CBT IO AggregateOrderBook
getTopOfBook prd = request $ "/products/" <> prd <> "/book?level=1"


getTopFiftyOfBook :: Text -> CBT IO AggregateOrderBook
getTopFiftyOfBook prd = request $ "/products/" <> prd <> "/book?level=2"


getFullOrderBook :: Text -> CBT IO OrderBook
getFullOrderBook prd = request $ "/products/" <> prd <> "/book?level=3"
