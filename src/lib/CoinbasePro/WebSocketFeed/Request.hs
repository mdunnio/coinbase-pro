{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CoinbasePro.WebSocketFeed.Request
  ( RequestMessageType (..)
  , ChannelName(..)
  , WebSocketFeedRequest (..)
  , AuthenticatedWebSocketFeedRequest

  , authenticatedWebSocketFeedRequest
  ) where

import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    object, withText, (.=))
import           Network.HTTP.Types                (methodGet)

import           CoinbasePro.Authenticated.Headers (CBAccessKey (..),
                                                    CBAccessPassphrase (..),
                                                    CBAccessSign (..),
                                                    CBAccessTimeStamp (..))
import           CoinbasePro.Authenticated.Request (CoinbaseProCredentials (..),
                                                    mkCBAccessSign,
                                                    mkCBAccessTimeStamp)
import           CoinbasePro.Request               (emptyBody,
                                                    encodeRequestPath)
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

data AuthenticatedWebSocketFeedRequest =
  AuthenticatedWebSocketFeedRequest WebSocketFeedRequest CBAccessSign CBAccessKey CBAccessPassphrase CBAccessTimeStamp


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

authenticatedWebSocketFeedRequest :: WebSocketFeedRequest
                                  -> CoinbaseProCredentials
                                  -> IO AuthenticatedWebSocketFeedRequest
authenticatedWebSocketFeedRequest wsRequest cpc = do
    ts <- liftIO mkCBAccessTimeStamp
    let cbs = mkCBAccessSign (cbSecretKey cpc) ts methodGet authSubscriptionPath emptyBody
    return $ AuthenticatedWebSocketFeedRequest wsRequest cbs (cbAccessKey cpc) (cbAccessPassphrase cpc) ts
  where
    authSubscriptionPath = encodeRequestPath ["users", "self", "verify"]
