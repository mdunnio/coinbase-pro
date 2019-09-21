{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                     (forever)
import qualified System.IO.Streams                 as Streams

import           CoinbasePro.Types                 (ProductId (..))
import           CoinbasePro.WebSocketFeed         (subscribeToFeed)
import           CoinbasePro.WebSocketFeed.Request (ChannelName (..))

main :: IO ()
main = do
    msgs <- subscribeToFeed [ProductId "BTC-USD"] [Ticker]
    forever $ Streams.read msgs >>= print
