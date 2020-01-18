{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class             (liftIO)

import           CoinbasePro.Authenticated
import           CoinbasePro.Authenticated.Accounts
import           CoinbasePro.Authenticated.Headers
import           CoinbasePro.Authenticated.Orders
import           CoinbasePro.Authenticated.Request
import           CoinbasePro.MarketData.Types       hiding (time)
import           CoinbasePro.Request
import           CoinbasePro.Types                  hiding (time)
import           CoinbasePro.Unauthenticated


main :: IO ()
main = do
    run (stats btcusd) >>= print
    run (candles btcusd Nothing Nothing Minute) >>= print
    run (trades btcusd) >>= print
    run time >>= print
    run products >>= print
    run (aggregateOrderBook btcusd (Just Best)) >>= print
    run (aggregateOrderBook btcusd (Just TopFifty)) >>= print
    run (fullOrderBook btcusd) >>= print
    runCbAuthT run cpc $ do
        accounts >>= liftIO . print
        account aid >>= liftIO . print
        fills (Just btcusd) Nothing >>= liftIO . print
        listOrders (Just [All]) (Just btcusd) >>= liftIO . print
        placeOrder Nothing btcusd Sell (Size 0.001) (Price 99999.00) True Nothing Nothing Nothing >>= liftIO . print
        placeOrder Nothing btcusd Buy (Size 1.0) (Price 1.00) True Nothing Nothing Nothing >>= liftIO . print
        cancelAll (Just btcusd) >>= liftIO . print
  where
    accessKey  = CBAccessKey "accesskey"
    secretKey  = CBSecretKey "secretkey"
    passphrase = CBAccessPassphrase "passphrase"
    cpc        = CoinbaseProCredentials accessKey secretKey passphrase
    aid        = AccountId "accountid"
    btcusd     = ProductId "BTC-USD"
