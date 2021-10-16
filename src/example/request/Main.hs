{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class             (liftIO)

import           CoinbasePro.Authenticated
import           CoinbasePro.Authenticated.Accounts
import           CoinbasePro.Authenticated.Headers
import           CoinbasePro.Authenticated.Orders
import           CoinbasePro.Authenticated.Request
import           CoinbasePro.Environment
import           CoinbasePro.MarketData.Types       hiding (time)
import           CoinbasePro.Request
import           CoinbasePro.Types                  hiding (time)
import           CoinbasePro.Unauthenticated


main :: IO ()
main = do
    run Sandbox $ do
        stats btcusd >>= liftIO . print
        candles btcusd Nothing Nothing Minute >>= liftIO . print
        trades btcusd >>= liftIO . print
        time >>= liftIO . print
        products >>= liftIO . print
        aggregateOrderBook btcusd (Just Best) >>= liftIO . print
        aggregateOrderBook btcusd (Just TopFifty) >>= liftIO . print
        fullOrderBook btcusd >>= liftIO . print
    runDefCbAuthT Sandbox cpc $ do
        accounts >>= liftIO . print
        account aid >>= liftIO . print
        accountHistory aid >>= liftIO . print
        fills (Just btcusd) Nothing >>= liftIO . print
        listOrders (Just [All]) (Just btcusd) >>= liftIO . print
        placeOrder Nothing btcusd Sell (Just $ Size 0.001) (Just $ Price 99999.00) (Just True) Nothing Nothing Nothing >>= liftIO . print
        placeOrder Nothing btcusd Buy (Just $ Size 1.0) (Just $ Price 1.00) (Just True) Nothing Nothing Nothing >>= liftIO . print
        cancelAll (Just btcusd) >>= liftIO . print
  where
    accessKey  = CBAccessKey "accesskey"
    secretKey  = CBSecretKey "secretkey"
    passphrase = CBAccessPassphrase "passphrase"
    cpc        = CoinbaseProCredentials accessKey secretKey passphrase
    aid        = AccountId "accountid"
    btcusd     = ProductId "BTC-USD"
