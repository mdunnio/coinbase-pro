{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel.Heartbeat
    ( Heartbeat (..)
    )where

import           Data.Aeson      (FromJSON (..), withObject, (.:))
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)


data Heartbeat = Heartbeat
    { sequence    :: Int
    , lastTradeId :: Int
    , productId   :: Text
    , time        :: UTCTime
    } deriving (Eq, Ord, Show)


instance FromJSON Heartbeat where
    parseJSON = withObject "heartbeat" $ \o ->
      Heartbeat <$>
        o .: "sequence" <*>
        o .: "last_trade_id" <*>
        o .: "product_id" <*>
        o .: "time"
