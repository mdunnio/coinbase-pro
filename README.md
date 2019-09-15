# coinbase-pro

## Request API

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class             (liftIO)

import           CoinbasePro.Authenticated
import           CoinbasePro.Authenticated.Accounts
import           CoinbasePro.Authenticated.Orders
import           CoinbasePro.Headers
import           CoinbasePro.MarketData.Types       hiding (time)
import           CoinbasePro.Request
import           CoinbasePro.Types                  hiding (time)
import           CoinbasePro.Unauthenticated


main :: IO ()
main = do
    stats btcusd >>= print
    candles btcusd Nothing Nothing Minute >>= print
    trades btcusd >>= print
    time >>= print
    products >>= print
    aggregateOrderBook btcusd (Just Best) >>= print
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


```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CoinbasePro.Types         (ProductId (..))
import           CoinbasePro.WebSocketFeed (subscribeToFeed)
import qualified System.IO.Streams         as Streams


main :: IO ()
main = do
    msgs <- subscribeToFeed [ProductId "BTC-USD"]
    forever $ Streams.read msgs >>= print

```
