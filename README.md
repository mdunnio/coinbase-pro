# coinbase-pro

## Request API

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class                     (liftIO)

import           CoinbasePro.Headers                        (CBAccessKey (..), CBAccessPassphrase (..))
import           CoinbasePro.MarketData.Types               (AggregateBookLevel (..),
                                                             FullBookLevel (..))
import           CoinbasePro.Request                        (CBSecretKey (..), CoinbaseProCredentials (..),
                                                             runCbAuthT)
import           CoinbasePro.Request.Authenticated          (account, accounts,
                                                             cancelAll,
                                                             cancelOrder, fills,
                                                             listOrders,
                                                             placeOrder)
import           CoinbasePro.Request.Authenticated.Accounts (AccountId (..))
import           CoinbasePro.Request.Authenticated.Orders   (Status (..))
import           CoinbasePro.Request.Unauthenticated        (aggregateOrderBook,
                                                             fullOrderBook,
                                                             products, time)
import           CoinbasePro.Types                          (OrderId (..),
                                                             Price (..),
                                                             ProductId (..),
                                                             Side (..),
                                                             Size (..))


main :: IO ()
main = do
    time >>= print
    products >>= print
    aggregateOrderBook btcusd (Just TopFifty) >>= print
    fullOrderBook btcusd (Just FullBookLevel) >>= print
    runCbAuthT cpc $ do
        accounts >>= liftIO . print
        account accountId >>= liftIO . print
        fills (Just btcusd) Nothing >>= liftIO . print
        listOrders (Just [All]) (Just btcusd) >>= liftIO . print
        placeOrder btcusd Sell (Size 0.001) (Price 99999.00) True Nothing Nothing Nothing >>= liftIO . print
        placeOrder btcusd Buy (Size 1.0) (Price 1.00) True Nothing Nothing Nothing >>= liftIO . print
        cancelOrder (OrderId "<cancel-order-id>")
        cancelAll (Just btcusd) >>= liftIO . print
  where
    accessKey  = CBAccessKey "<access-key>"
    secretKey  = CBSecretKey "<secret-key>"
    passphrase = CBAccessPassphrase "<passphrase>"
    cpc        = CoinbaseProCredentials accessKey secretKey passphrase
    accountId  = AccountId "<account-id>"
    btcusd     = ProductId "BTC-USD"
```

## Websocket API

To print out all of the full order book updates for BTC-USD:


```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CoinbasePro.Types                 (ProductId (..))
import           CoinbasePro.WebSocketFeed         (parseFeed)
import           CoinbasePro.WebSocketFeed.Request (ChannelName (..),
                                                    RequestMessageType (..),
                                                    WebSocketFeedRequest (..),
                                                    wsEndpoint)
import qualified CoinbasePro.WebSocketFeed.Request as WR
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson                        (encode)
import qualified Network.WebSockets                as WS
import qualified Wuss                              as WU


main :: IO ()
main =
    WU.runSecureClient wsHost wsPort "/" $ \conn -> do
        WS.sendTextData conn $ encode request
        liftIO . forever $ liftIO (parseFeed conn) >>= print
  where
    wsHost = WR.host wsEndpoint
    wsPort = WR.port wsEndpoint

    request = WebSocketFeedRequest Subscribe [prids] [Full]
    prids = ProductId "BTC-USD"

```
