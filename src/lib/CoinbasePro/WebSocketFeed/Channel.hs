{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Channel
  ( ChannelMessage (..)
  ) where

import           Data.Aeson                                      (FromJSON (..),
                                                                  Value (..),
                                                                  withObject,
                                                                  (.:))

import           CoinbasePro.WebSocketFeed.Channel.Full.Activate (Activate (..))
import           CoinbasePro.WebSocketFeed.Channel.Full.Change   (Change (..))
import           CoinbasePro.WebSocketFeed.Channel.Full.Done     (Done (..))
import           CoinbasePro.WebSocketFeed.Channel.Full.Match    (Match (..))
import           CoinbasePro.WebSocketFeed.Channel.Full.Open     (Open (..))
import           CoinbasePro.WebSocketFeed.Channel.Full.Received (Received (..))
import           CoinbasePro.WebSocketFeed.Channel.Heartbeat     (Heartbeat (..))
import           CoinbasePro.WebSocketFeed.Channel.Level2        (L2Update (..),
                                                                  Snapshot (..))
import qualified CoinbasePro.WebSocketFeed.Channel.Level2        as L2
import           CoinbasePro.WebSocketFeed.Channel.Ticker        (Ticker (..))
import           CoinbasePro.WebSocketFeed.Response              (Subscription)

data ChannelMessage =
      ActivateMessage Activate
    | ChangeMessage Change
    | DoneMessage Done
    | HeartbeatMessage Heartbeat
    | L2ChangeMessage L2.Change
    | L2SnapshotMessage Snapshot
    | L2UpdateMessage L2Update
    | MatchMessage Match
    | OpenMessage Open
    | ReceivedMessage Received
    | TickerMessage Ticker
    | SubscriptionMessage Subscription
    deriving (Show, Eq)


instance FromJSON ChannelMessage where
    parseJSON = withObject "channel message" $ \o -> do
        t <- String <$> o .: "type"
        case t of
          "activate"      -> ActivateMessage <$> parseJSON (Object o)
          "change"        -> ChangeMessage <$> parseJSON (Object o)
          "done"          -> DoneMessage <$> parseJSON (Object o)
          "heartbeat"     -> HeartbeatMessage <$> parseJSON (Object o)
          "l2update"      -> L2UpdateMessage <$> parseJSON (Object o)
          "last_match"    -> MatchMessage <$> parseJSON (Object o)
          "match"         -> MatchMessage <$> parseJSON (Object o)
          "open"          -> OpenMessage <$> parseJSON (Object o)
          "received"      -> ReceivedMessage <$> parseJSON (Object o)
          "snapshot"      -> L2SnapshotMessage <$> parseJSON (Object o)
          "subscriptions" -> SubscriptionMessage <$> parseJSON (Object o)
          "ticker"        -> TickerMessage <$> parseJSON (Object o)
          _               -> fail "Unable to parse channel message"
