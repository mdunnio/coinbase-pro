{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Request
  ( RequestMessageType (..)
  , ChannelName(..)
  , WebSocketFeedRequest (..)
  , AuthenticatedWebSocketFeedRequest (..)
  ) where

import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    object, withText, (.=))

import           CoinbasePro.Authenticated.Headers (CBAccessKey (..),
                                                    CBAccessPassphrase (..),
                                                    CBAccessSign (..),
                                                    CBAccessTimeStamp (..))
import           CoinbasePro.Types                 (ProductId)


data RequestMessageType = Subscribe | Unsubscribe
    deriving (Eq, Ord)


instance Show RequestMessageType where
    show Subscribe   = "subscribe"
    show Unsubscribe = "unsubscribe"


data ChannelName = Heartbeat | Status | Ticker | Level2 | Matches | Full
    deriving (Eq, Ord)


instance Show ChannelName where
    show Heartbeat = "heartbeat"
    show Status    = "status"
    show Ticker    = "ticker"
    show Level2    = "level2"
    show Matches   = "matches"
    show Full      = "full"


instance ToJSON ChannelName where
    toJSON Heartbeat = toJSON $ show Heartbeat
    toJSON Status    = toJSON $ show Status
    toJSON Ticker    = toJSON $ show Ticker
    toJSON Level2    = toJSON $ show Level2
    toJSON Matches   = toJSON $ show Matches
    toJSON Full      = toJSON $ show Full


instance FromJSON ChannelName where
    parseJSON = withText "channel name" $ \case
        "heartbeat" -> return Heartbeat
        "status"    -> return Status
        "ticker"    -> return Ticker
        "level2"    -> return Level2
        "matches"   -> return Matches
        "full"      -> return Full
        _           -> fail "Unable to parse channel"


data WebSocketFeedRequest = WebSocketFeedRequest
    { reqMsgType    :: RequestMessageType
    , reqProductIds :: [ProductId]
    , reqChannels   :: [ChannelName]
    } deriving (Eq, Ord, Show)


instance ToJSON WebSocketFeedRequest where
    toJSON (WebSocketFeedRequest rmt rpi rc) = object
        [ "type" .= show rmt
        , "product_ids" .= rpi
        , "channels" .= rc
        ]

data AuthenticatedWebSocketFeedRequest = AuthenticatedWebSocketFeedRequest
    { request      :: WebSocketFeedRequest
    , cbSig        :: CBAccessSign
    , cbKey        :: CBAccessKey
    , cbPassphrase :: CBAccessPassphrase
    , cbTimestamp  :: CBAccessTimeStamp
    } deriving (Eq)


instance ToJSON AuthenticatedWebSocketFeedRequest where
    toJSON (AuthenticatedWebSocketFeedRequest req s k p t) = object
        [ "type" .= show (reqMsgType req)
        , "product_ids" .= reqProductIds req
        , "channels" .= reqChannels req
        , "signature" .= s
        , "key" .= k
        , "passphrase" .= p
        , "timestamp" .= t
        ]
