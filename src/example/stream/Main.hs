{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                     (forever)
import qualified System.IO.Streams                 as Streams

import           CoinbasePro.Environment           (Environment (..))
import           CoinbasePro.Types                 (ProductId (..))
import           CoinbasePro.WebSocketFeed         (subscribeToFeed)
import           CoinbasePro.WebSocketFeed.Request (ChannelName (..))


main :: IO ()
main = do
    msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
    forever $ Streams.read msgs >>= print
